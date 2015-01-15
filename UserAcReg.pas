unit UserAcReg;

interface
uses Windows, SysUtils, Classes;

function GetLastInputTick: DWord; // TickCount at the moment of last input
function GetUnixTime: Int64;


function GetIdleCount: DWord;     // Idle millisecconds (resolution ~10 - 16 ms.)
function GetIdleState: Boolean;   // Current state - idle/acctive
function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord=0): boolean;


type TUserActivityCounter = class(TObject)
  private
     {
       Time is an elapsed amount, but Tick is a point in time.

     }

     FBusyTime   : DWord;    // Acumulated Busy Time
     FPresentTime: DWord;    // Acumulated Present Time

     FStartTick  : DWord;    // Tick Count at which started counting
     FLastTick   : DWord;    // Last Registered Tick
     FLITick     : DWord;    // Last Registered Last Input Tick
     FBChangeTick: DWord;    // Last change to Busy state
     FIChangeTick: DWord;    // Last change to Idle state 
     FPChangeTick: DWord;    // Last change to Present state 
     FAChangeTick: DWord;    // Last change to Absent state 

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
     OnStateChange  : TNotifyEvent; // Called when get busy or idle
     OnBusyChange   : TNotifyEvent; // Called when get busy or idle
     OnPresentChange: TNotifyEvent; // Called when get present or absent
     OnBusy         : TNotifyEvent; // Called when get busy
     OnIdle         : TNotifyEvent; // Called when get idle
     OnPresent      : TNotifyEvent; // Called when get present
     OnAbsent       : TNotifyEvent; // Called when get absent
     
     constructor Create;
     destructor  Destroy; override;
     
     function    Update: boolean;   // Call this method as often as posible to monitore state changes

     property AbsentTimeout: DWord read FAbsentTO write SetAbsentTimeout; // After this amount of msec. become absent
     property IdleTimeout  : DWord read FIdleTO   write SetIdleTimeout;   // After this amount of msec. become idle

     property StartTick : DWord read FStartTick;
     property LastTick  : DWord read FLastTick;

     // Totals:
     function TotalTime  : DWord; // Total elapsed time since started monitoring
     
     function IdleTime   : DWord; // Amount of Idle    time (msec.)
     function BusyTime   : DWord; // Amount of Busy    time
     function AbsentTime : DWord; // Amount of Absent  time
     function PresentTime: DWord; // Amount of Present time

     // Last activity session:
     function IdleTimeLast   : DWord; // Amount of time of last idle    session
     function BusyTimeLast   : DWord; // Amount of time of last busy    session
     function AbsentTimeLast : DWord; // Amount of time of last absent  session
     function PresentTimeLast: DWord; // Amount of time of last present session

     function Present: boolean;   // Presence state
     function Busy   : boolean;   // Busy state
     function State  : Integer;   // 0 - Absent, 1 - Present and Idle, 2 - Busy (and Present)
     
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
  i := i xor i;
  FIdleTO      := 100;
  FAbsentTO    := i;
  FBusyTime    := i;
  FPresentTime := i;
  
  OnStateChange  := nil;
  OnBusyChange   := OnStateChange;
  OnPresentChange:= OnStateChange;
  OnBusy         := OnStateChange;
  OnIdle         := OnStateChange;
  OnPresent      := OnStateChange;
  OnAbsent       := OnStateChange;

  i := GetTickCount;
  FStartTick   := i;
  FBChangeTick := i;
  FIChangeTick := i;
  FPChangeTick := i;
  FAChangeTick := i;
  
  i := GetLastInputTick;

  FLITick      := 0;
  FLastTick    := GetTickCount;

  FBusy := FLastTick < i + FIdleTO;
  FPresent := FBusy;

//   if FLITick < FStartTick and FBusy then FLITick := FStartTick;
end;

destructor TUserActivityCounter.Destroy;
begin
  Self.Update;
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
   if Assigned(OnStateChange) then OnStateChange(Self);
   if Assigned(OnBusyChange) then OnBusyChange(Self);
   if FBusy then begin 
      if Assigned(OnBusy) then OnBusy(Self);
   end else begin
      if Assigned(OnIdle) then OnIdle(Self);
   end    
end;

procedure TUserActivityCounter.PresentChanged;
begin
   if not FPresent and Assigned(OnStateChange) then OnStateChange(Self);
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
  
  if cb then begin
     FBusy := not FBusy;
     if cb then FBChangeTick := tk else FIChangeTick := tk;
  end;

  if cp then begin
     FPresent := not FPresent;
     if cp then FPChangeTick := tk else FAChangeTick := tk;
  end;
  
  if cb then BusyChanged;
  if cp then PresentChanged;
  
  Result := cb or cp;
end;

function TUserActivityCounter.BusyTimeLast: DWord;
begin
   if FBusy then Result := GetTickCount else Result := FIChangeTick;
   dec(Result, FBChangeTick); 
end;

function TUserActivityCounter.IdleTimeLast: DWord;
begin
   if FBusy then Result := FBChangeTick else Result := GetTickCount;
   dec(Result, FIChangeTick); 
end;

function TUserActivityCounter.PresentTimeLast: DWord;
begin
   if FPresent then Result := GetTickCount else Result := FAChangeTick;
   dec(Result, FPChangeTick); 
end;

function TUserActivityCounter.AbsentTimeLast: DWord;
begin
   if FPresent then Result := FPChangeTick else Result := GetTickCount;
   dec(Result, FAChangeTick); 
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

function TUserActivityCounter.State: Integer;
begin
  Result := Result xor Result;
  inc(Result, Integer(FPresent));
  inc(Result, Integer(FBusy));
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
