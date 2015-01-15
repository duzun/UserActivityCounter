unit UserAcReg;

interface
uses Windows, SysUtils;

function GetLastInputTick: DWord; // TickCount at the moment of last input
function GetUnixTime: Int64;


function GetIdleCount: DWord;     // Idle millisecconds (resolution ~10 - 16 ms.)
function GetIdleState: Boolean;   // Current state - idle/acctive
function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord=0): boolean;


type TUserActivityCounter = class(TObject)
  private
     {
       Time is an amount elapsed, but Tick is a point in time.

     }

     FBusyTime   : DWord;    // Acumulated Busy Time
     FPresentTime: DWord;    // Acumulated Present Time

     FStartTick  : DWord;    // Tick Count at which started counting
     FLastTick   : DWord;    // Last Registered Tick
     FLITick     : DWord;    // Last Registered Last Input Tick
     FBChangeTick: DWord;    // Last Busy/Idle state change
     FPChangeTick: DWord;    // Last Present state change

     FBusy       : boolean;  // Busy state
     FPresent    : boolean;  // Present state

    procedure SetAbsentTimeout(const Value: DWord);
    procedure SetIdleTimeout(const Value: DWord);

  protected
     FIdleTO     : DWord;    // Idle timeout
     FAbsentTO   : DWord;    // Absent timeout

     procedure BusyChanged;
     procedure PresentChanged;

  public
     OnBusyChange   : procedure (Sender: TObject); // Called when get busy or idle
     OnPresentChange: procedure (Sender: TObject); // Called when get present or absent
     OnBusy         : procedure (Sender: TObject); // Called when get busy
     OnIdle         : procedure (Sender: TObject); // Called when get idle
     OnPresent      : procedure (Sender: TObject); // Called when get present
     OnAbsent       : procedure (Sender: TObject); // Called when get absent
     
     constructor Create;
     destructor  Destroy; override;
     
     function    Update: boolean;   // Call this method as often as posible to monitore state changes

     property AbsentTimeout: DWord read FAbsentTO write SetAbsentTimeout; // After this amount of msec. become absent
     property IdleTimeout  : DWord read FIdleTO   write SetIdleTimeout;   // After this amount of msec. become idle

     function StartTime  : DWord; // Tick Count at which started monitoring

     // Totals:
     function TotalTime  : DWord; // Total elapsed time since started monitoring
     function IdleTime   : DWord; // Amount of Idle time (msec.)
     function BusyTime   : DWord; // Amount of Busy time
     function AbsentTime : DWord; // Amount of Absent time
     function PresentTime: DWord; // Amount of Present time

     // Last activity session:
     function PresentSes: DWord; // Amount of time of last present session
     function AbsentSes : DWord; // Amount of time of last absent session
     function BusySes   : DWord; // Amount of time of last busy session
     function IdleSes   : DWord; // Amount of time of last idle session

     function Present: boolean;   // Presence state
     function Busy   : boolean;   // Busy state
     
end;

// Deprecated
var UAInactiveTimeout: DWord;
var UAState: Boolean; // true - idle, false - active

var FormatSettings: TFormatSettings;


implementation

uses DateUtils;
var liInfo: TLastInputInfo;

function GetLastInputTick: DWord;
begin
   GetLastInputInfo(liInfo);
   Result := liInfo.dwTime;
end;

function GetUnixTime: Int64;
begin
   Result := DateTimeToUnix(Date+Time);
end;

function min(a,b:DWord):DWord;begin if a < b then Result := a else Result := b end;
function max(a,b:DWord):DWord;begin if a < b then Result := b else Result := a end;

{ TUserActivityCounter }

function TUserActivityCounter.AbsentTime: DWord;
begin
   Result := TotalTime;
   dec(Result, FPresentTime);
end;

function TUserActivityCounter.BusyTime: DWord;
begin
   Result := FBusyTime;
end;

constructor TUserActivityCounter.Create;
var i: DWord;
begin
  inherited Create;
  i := GetLastInputTick;
  FStartTick   := GetTickCount;

  FIdleTO      := 100;
  FAbsentTO    := 0;
  FBusyTime    := 0;
  FPresentTime := 0;
  
  OnBusyChange   := nil;
  OnPresentChange:= nil;
  OnBusy         := nil;
  OnIdle         := nil;
  OnPresent      := nil;
  OnAbsent       := nil;

  FLITick      := 0;
  FLastTick    := GetTickCount;

  FBusy := FLastTick < i + FIdleTO;
  FPresent := FBusy;
  
//   if FLITick < FStartTick and FBusy then FLITick := FStartTick;
end;

destructor TUserActivityCounter.Destroy;
begin

  inherited;
end;

function TUserActivityCounter.IdleTime: DWord;
begin
   Result := TotalTime;
   dec(Result, FBusyTime);
end;

function TUserActivityCounter.Present: boolean;
begin
   Result := boolean(FPresent);
end;

function TUserActivityCounter.PresentTime: DWord;
begin
   Result := FPresentTime;
end;

function TUserActivityCounter.TotalTime: DWord;
begin
   Result := GetTickCount;
   dec(Result, FStartTick);
end;

function TUserActivityCounter.StartTime: DWord;
begin
   Result := FStartTick;
end;

procedure TUserActivityCounter.SetAbsentTimeout(const Value: DWord);
begin
  FAbsentTO := Value;
end;

procedure TUserActivityCounter.SetIdleTimeout(const Value: DWord);
begin
  FIdleTO := Value;
end;

procedure TUserActivityCounter.BusyChanged;
begin
   if Assigned(OnBusyChange) then OnBusyChange(Self);
   if FBusy then begin 
      if Assigned(OnBusy) then OnBusy(Self);
   end else begin
      if Assigned(OnIdle) then OnIdle(Self);
   end    
end;

procedure TUserActivityCounter.PresentChanged;
begin
   if Assigned(OnPresentChange) then OnPresentChange(Self);
   if FPresent then begin 
      if Assigned(OnPresent) then OnPresent(Self);
   end else begin
      if Assigned(OnAbsent) then OnAbsent(Self);
   end    
end;

function TUserActivityCounter.Busy: boolean;
begin
   Result := boolean(FBusy);
end;

function TUserActivityCounter.Update: boolean;
var tk, li: DWord;
    cp, cb: boolean;
begin
// li 1 ito 2 li li 3 ito 4 5 6 
  li := GetLastInputTick;
  tk := GetTickCount; 
  cb := false;
  cp := false;
  
  if FLastTick < li then begin  // new input
     if li <= FLITick + FIdleTO then begin
        inc(FBusyTime, li-FLITick);
        cb := true;
     end;
     
     if li <= FLITick + FAbsentTO then begin
        inc(FPresentTime, li-FLITick);
        cp := true;
     end;
     
     FLITick := li; // last registered user activity
  end else begin // no new input
  
  end;
  
  FLastTick := tk;
  
  cb := FBusy <> cb;
  cp := FPresent <> cp;
  if cb then FBusy := not FBusy;
  if cp then FPresent := not FPresent;
  
  if cb then BusyChanged;
  if cp then PresentChanged;
  
  Result := cb or cp;
end;



function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord): boolean;
begin
// li	to	lti	li	lta	[li]	to	lti ...
   if IdleTimeout < 10 then IdleTimeout := UAInactiveTimeout;
   Result := (not WasIdle) and (GetIdleCount >= IdleTimeout) or
                  WasIdle  and (GetLastInputTick > LastTick);
end;

function GetIdleState: Boolean;
begin
   GetIdleCount;
   Result := UAState;
end;

function GetIdleCount: DWord;
begin
   Result := GetTickCount;
   dec(Result,GetLastInputTick);
   UAState := Result >= UAInactiveTimeout;
end;


function TUserActivityCounter.AbsentSes: DWord;
begin

end;

function TUserActivityCounter.BusySes: DWord;
begin

end;

function TUserActivityCounter.IdleSes: DWord;
begin

end;

function TUserActivityCounter.PresentSes: DWord;
begin

end;

initialization
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   UAInactiveTimeout := 100;

   GetIdleCount;

  {Setarile implicite pentru formatarea numerelor reale}
  GetLocaleFormatSettings(0, FormatSettings);
  FormatSettings.DecimalSeparator := '.';  {Indiferent de setarile din sistem, separatorul zecimal va fi '.'}
  FormatSettings.ShortTimeFormat := 'mm:ss';
  FormatSettings.LongTimeFormat := 'hh:mm:ss';

end.
