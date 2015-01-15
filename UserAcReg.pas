unit UserAcReg;

interface
uses Windows, SysUtils, Classes;

function GetLastInputTick: DWord; // TickCount at the moment of last input
function GetUnixTime: Int64;

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

  public
     OnStateChange  : TNotifyEvent; // Called when get busy or idle
     OnBusyChange   : TNotifyEvent; // Called when get busy or idle
     OnPresentChange: TNotifyEvent; // Called when get present or absent
     OnBusy         : TNotifyEvent; // Called when get busy
     OnIdle         : TNotifyEvent; // Called when get idle
     OnPresent      : TNotifyEvent; // Called when get present
     OnAbsent       : TNotifyEvent; // Called when get absent

     procedure   Reset;
     constructor Create;
     destructor  Destroy; override;

     function    Update: boolean;   // Call this method as often as posible to monitore state changes

     procedure BusyChanged(Sender: TObject=nil);
     procedure PresentChanged(Sender: TObject=nil);

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
     function IdleTimeLast   : DWord;                       // Amount of time of last idle    session
     function BusyTimeLast(Potent:boolean=false)   : DWord; // Amount of time of last busy    session
     function AbsentTimeLast : DWord;                       // Amount of time of last absent  session
     function PresentTimeLast(Potent:boolean=false): DWord; // Amount of time of last present session

     function Present: boolean;   // Presence state
     function Busy   : boolean;   // Busy state
     function State  : Integer;   // 0 - Absent, 1 - Present and Idle, 2 - Busy (and Present)

end;

var FormatSettings: TFormatSettings;


implementation

uses DateUtils;
var liInfo: TLastInputInfo;

function GetLastInputTick: DWord;
begin
   try
     GetLastInputInfo(liInfo);
     Result := liInfo.dwTime;
   except
     Result := 0;
   end;
end;

function GetUnixTime: Int64; begin Result := DateTimeToUnix(Date+Time); end;

function min(a,b:DWord):DWord;begin if a < b then Result := a else Result := b end;
function max(a,b:DWord):DWord;begin if a < b then Result := b else Result := a end;

{ TUserActivityCounter }

constructor TUserActivityCounter.Create;
var i: DWord;
begin
  inherited Create;
  
  i := i xor i;
  FIdleTO      := 128;
  FAbsentTO    := i;

  Self.Reset;
  
  // ??? should reset?
  FLITick      := i;
  FLastTick    := i;
  FStartTick   := i;

  OnStateChange  := nil;
  OnBusyChange   := OnStateChange;
  OnPresentChange:= OnStateChange;
  OnBusy         := OnStateChange;
  OnIdle         := OnStateChange;
  OnPresent      := OnStateChange;
  OnAbsent       := OnStateChange;
end;

destructor TUserActivityCounter.Destroy;
begin
  Self.Update;
  inherited;
end;

procedure TUserActivityCounter.Reset;
var i: DWord;
begin
  i := i xor i;
  // ??? should reset?
  FLITick      := i;
  FLastTick    := i;
  FStartTick   := i;

  // Need reset:
  FBusyTime    := i;
  FPresentTime := i;

  FBChangeTick := i;
  FIChangeTick := i;
  FPChangeTick := i;
  FAChangeTick := i;

  FBusy    := false;
  FPresent := false;
end;

function TUserActivityCounter.Update: boolean; // @return state shanged since last run
var tk, li: DWord;
    cp, cb: boolean;
    first: boolean;
begin
  li := GetLastInputTick; // = 0 on error or no input at all
  tk := GetTickCount;
  
  cb := tk < li + FIdleTO;
  cp := tk < li + FAbsentTO; // computed presence

  first := FStartTick = 0;
  if first then begin
      if cp then begin
         FStartTick := li
      end else begin
         li := 0;
         FStartTick := tk
      end;
      FBusy    := cb;
      FPresent := cp;
      
      FBChangeTick := FStartTick;
      FIChangeTick := FStartTick;
      FPChangeTick := FStartTick;
      FAChangeTick := FStartTick;
  end;
  
  if FLastTick < li then begin  // new input
     if li <= FLITick + FIdleTO then begin
        inc(FBusyTime, li-FLITick);
     end;

     if li <= FLITick + FAbsentTO then begin
        inc(FPresentTime, li-FLITick);
     end;

     FLITick := li; // last registered user activity
  end else begin // no new input

  end;

  FLastTick := tk;

  cb := FBusy <> cb;      // busy state changed
  cp := FPresent <> cp;   // present state changed

  if cb then begin
     FBusy := not FBusy;
     if FBusy then FBChangeTick := li else FIChangeTick := li;
  end;

  if cp then begin
     FPresent := not FPresent;
     if FPresent then FPChangeTick := li else FAChangeTick := li;
  end;

  if cb then BusyChanged;
  if cp then PresentChanged;

  Result := cb or cp;
end;

function TUserActivityCounter.PresentTime: DWord; begin
   Result := FPresentTime;
end;

function TUserActivityCounter.AbsentTime: DWord; begin
   Result := TotalTime;
   dec(Result, FPresentTime);
end;

function TUserActivityCounter.BusyTime: DWord; begin
   Result := FBusyTime;
end;

function TUserActivityCounter.IdleTime: DWord; begin
   Result := TotalTime;
   dec(Result, FBusyTime);
end;

function TUserActivityCounter.TotalTime: DWord; begin
   Result := GetTickCount;
   dec(Result, FStartTick);
end;

function TUserActivityCounter.PresentTimeLast(Potent:boolean): DWord;
begin
   if Potent and FPresent then Result := GetTickCount
                          else Result := FLITick;
   if Result > 0 then dec(Result, FPChangeTick);
end;

function TUserActivityCounter.AbsentTimeLast: DWord;
begin
   if FPresent then Result := FPChangeTick 
               else Result := GetTickCount;
   dec(Result, FAChangeTick);
end;

function TUserActivityCounter.BusyTimeLast(Potent:boolean): DWord;
begin
   if Potent and FBusy then Result := GetTickCount
                       else Result := FLITick;
   if Result > 0 then dec(Result, FBChangeTick);
end;

function TUserActivityCounter.IdleTimeLast: DWord;
begin
   if FBusy then Result := FBChangeTick 
            else Result := GetTickCount;
   dec(Result, FIChangeTick);
end;

procedure TUserActivityCounter.BusyChanged;
begin
   if Sender = nil then Sender := Self;
   if Assigned(OnStateChange) then OnStateChange(Sender);
   if Assigned(OnBusyChange)  then OnBusyChange(Sender);
   if FBusy then begin   if Assigned(OnBusy) then OnBusy(Sender);
   end else begin        if Assigned(OnIdle) then OnIdle(Sender);
   end
end;

procedure TUserActivityCounter.PresentChanged;
begin
   if Sender = nil then Sender := Self;
   if not FPresent and Assigned(OnStateChange) then OnStateChange(Sender);
   if Assigned(OnPresentChange) then OnPresentChange(Sender);
   if FPresent then begin   if Assigned(OnPresent) then OnPresent(Sender);
   end else begin           if Assigned(OnAbsent)  then OnAbsent(Sender);
   end
end;

function TUserActivityCounter.State: Integer;
begin
  Result := Result xor Result;
  inc(Result, Integer(FPresent));
  inc(Result, Integer(FBusy));
end;

function TUserActivityCounter.Busy: boolean;    begin Result := boolean(FBusy);    end;
function TUserActivityCounter.Present: boolean; begin Result := boolean(FPresent); end;


procedure TUserActivityCounter.SetAbsentTimeout(const Value: DWord); begin FAbsentTO := Value; end;
procedure TUserActivityCounter.SetIdleTimeout(const Value: DWord); begin FIdleTO := Value; end;

initialization
  liInfo.cbSize := SizeOf(TLastInputInfo) ;

  {Setarile implicite pentru formatarea numerelor reale}
  GetLocaleFormatSettings(0, FormatSettings);
  FormatSettings.DecimalSeparator := '.';  {Indiferent de setarile din sistem, separatorul zecimal va fi '.'}
  FormatSettings.ShortTimeFormat := 'mm:ss';
  FormatSettings.LongTimeFormat := 'hh:mm:ss';

  FormatSettings.ShortDateFormat := 'dd.MM.yyyy';
  FormatSettings.DateSeparator := '.';

end.