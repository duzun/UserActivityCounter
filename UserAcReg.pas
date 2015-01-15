unit UserAcReg;

interface
uses Windows, SysUtils;

function GetLastInputTick: DWord; // TickCount at the moment of last input
function GetIdleCount: DWord;   // Idle millisecconds (resolution ~10 - 16 ms.)
function GetIdleState: Boolean; // Current state - idle/acctive
function GetUnixTime: Int64;

function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord=0): boolean;

type TUserActivityCounter = class(TObject) 
  private
     FBusyTime, FPresentTime: ULong; // Busy, Present Time
     FIdleTO   : DWord;    // Idle timeout
     FAbsentTO : DWord;    // Absent timeout  
     
     FStartTick: DWord;    // Tick Count at which started counting
     FLastTick : DWord;    // Last Registered Tick
     FLITick   : DWord;    // Last Registered Last Input Tick
     FBusy     : boolean;  // Busy state
     FPresent  : boolean;  // Present state
     
  protected
     procedure StateChanged(tk: DWord);
     
  public
     constructor Create;
     destructor  Destroy; override;
     function    Update: boolean;

     function IdleTime: DWord;
     function BusyTime: DWord;
     function AbsentTime: DWord;
     function PresentTime: DWord;
     function TotalTime: DWord;

     function Present: boolean;
     function Busy: boolean;
     
end;

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

function GetUnixTime: Int64;
begin
   Result := DateTimeToUnix(Date+Time);
end;

{ TUserActivityCounter }

function TUserActivityCounter.AbsentTime: DWord;
begin
   Result := TotalTime;
   dec(Result, FPresentTime);
end;

function TUserActivityCounter.Busy: boolean;
begin
   Result := FBusy;
end;

function TUserActivityCounter.BusyTime: DWord;
begin
   Result := FBusyTime;
end;

constructor TUserActivityCounter.Create;
begin
  inherited Create;
  FIdleTO      := 100;
  FBusyTime    := 0;
  FPresentTime := 0;
  FLITick      := GetLastInputTick;
  FLastTick    := GetTickCount;
  FStartTick   := FLastTick;
  FBusy := FLastTick - FLITick < FIdleTO;
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
   Result := FPresent;
end;

function TUserActivityCounter.PresentTime: DWord;
begin
   Result := FPresentTime;
end;

procedure TUserActivityCounter.StateChanged;
var i: DWord;
begin
  if FBusy then begin
    inc(FBusyTime, tk - FLITick);
  end else begin
    if FLastTick < FLITick then
       inc(FBusyTime, FLITick - FLastTick);
  end;

  FLastTick := tk;
  FBusy := not FBusy;
end;

function TUserActivityCounter.TotalTime: DWord;
begin
   Result := GetTickCount;
   dec(Result, FStartTick);
end;

function TUserActivityCounter.Update: boolean;
var tk: DWord;
begin
  FLITick := GetLastInputTick;
  tk := GetTickCount;
  Result := tk - FLITick < FIdleTO;
  if Result <> FBusy then StateChanged(tk);
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
