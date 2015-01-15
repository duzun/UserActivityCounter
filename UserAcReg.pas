unit UserAcReg;

interface
uses Windows, SysUtils;

function GetLastInputTick: DWord; // TickCount at the moment of last input
function GetIdleCount: DWord;   // Idle millisecconds (resolution ~10 - 16 ms.)
function GetIdleState: Boolean; // Current state - idle/acctive
function GetUnixTime: Int64;
function IdleTick: DWord;

function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord=0): boolean;

var UAResolution: word;
var UAInactiveTimeout: DWord;
var UAState: Boolean; // true - idle, false - active

var FormatSettings: TFormatSettings;

implementation

uses DateUtils;
var liInfo: TLastInputInfo;

function LastInputStateChanged(WasIdle: boolean; LastTick: DWord; IdleTimeout: DWord): boolean;
begin
// li	to	lti	li	lta	[li]	to	lti ...
   if IdleTimeout < 10 then IdleTimeout := UAInactiveTimeout;
   Result := (not WasIdle) and (GetIdleCount >= IdleTimeout) or
                  WasIdle  and (GetLastInputTick > LastTick);
end;

function GetIdleState: Boolean; begin GetIdleCount; Result := UAState; end;

function GetLastInputTick: DWord;
begin
   GetLastInputInfo(liInfo) ;
   Result := liInfo.dwTime;
end;

function GetIdleCount: DWord;
begin
   Result := GetTickCount;
   dec(Result,GetLastInputTick);
   UAState := Result >= UAInactiveTimeout;
end;

function IdleTick: DWord;
begin
   Result := GetIdleCount;
   if(UAResolution>1) then
      Result := Result div UAResolution;
end;

function GetUnixTime: Int64;
begin
   Result := DateTimeToUnix(Date+Time);
end;

initialization
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   UAResolution := 1;
   UAInactiveTimeout := 100;

   GetIdleCount;

  {Setarile implicite pentru formatarea numerelor reale}
  GetLocaleFormatSettings(0, FormatSettings);
  FormatSettings.DecimalSeparator := '.';  {Indiferent de setarile din sistem, separatorul zecimal va fi '.'}
  FormatSettings.ShortTimeFormat := 'mm:ss';
  FormatSettings.LongTimeFormat := 'hh:mm:ss';

end.
