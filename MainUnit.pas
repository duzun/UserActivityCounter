unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  UserAcReg, StdCtrls, ActnList, ComCtrls, ShellApi, TrayIcon;

const WM_ICONTRAY = WM_USER + 1;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LAt: TLabel;
    LIt: TLabel;
    ActionList1: TActionList;
    ShowInfo: TAction;
    Label1: TLabel;
    Label2: TLabel;
    LITO: TLabel;
    LAt_: TLabel;
    LIt_: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OnStateChange_: TAction;
    EITO: TEdit;
    Label7: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    StatusBar1: TStatusBar;
    BSetI: TButton;
    OnStateChange: TAction;
    SetIdle: TAction;
    HideToTray: TAction;
    ShowFromTray: TAction;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ShowInfoExecute(Sender: TObject);
    procedure EITOChange(Sender: TObject);
    procedure OnStateChange_Execute(Sender: TObject);
    procedure OnStateChangeExecute(Sender: TObject);
    procedure SetIdleExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HideToTrayExecute(Sender: TObject);
    procedure ShowFromTrayExecute(Sender: TObject);
    procedure TrayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure TrayMessage(var Msg: TMessage); message WM_ICONTRAY;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  private
    { Private declarations }
    TrayIcon: TTrayIcon;

    FormCaptStr: String;
    tk: DWord;   // TickCount
    litk: DWord; // LastInputTickCount
    lt, lt_: DWord; // LastTick
    _Idle: Boolean;
    StartDate: TDateTime;

    _toHide: boolean;
    
  public
    { Public declarations }
    InactiveTime: ULong;
    InactiveTime_: ULong;
    ActiveTime: ULong;
    ActiveTime_: ULong;
    IdleTimeout: DWord;
    LastState, LastState_: Boolean; // true - active, false - idle

    UAC: TUserActivityCounter;
  end;

var
  Form1: TForm1;


implementation

uses DateUtils, Math;

{$R *.dfm}

function MSec2StrTime(msec:ULong):string;
var d: TDateTime;
begin
   d := msec / MSecsPerDay;
   Result := '';
   if d >= 1 then Result := IntToStr(Floor(d)) + '-';
   Result := Result + TimeToStr(d, FormatSettings);
end;

function OnWhichPanel(Sender: TStatusBar; X: integer): integer;
var i, n: integer;
begin
   i := 0;
   Result := 0;
   n := Sender.Panels.Count;
   while Result < n do begin
      inc(i, Sender.Panels[Result].Width);
      if x < i then Exit;
      inc(Result);
   end;
   dec(Result);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
     #27: Self.Close;
   end;  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrayIcon := TTrayIcon.Create(Self);
  TrayIcon.Hint := Caption;
  TrayIcon.OnMouseDown := TrayMouseDown;

   _toHide := ParamStr(1) = '/min';

   UAC := TUserActivityCounter.Create;

   EITOChange(EITO);
   FormCaptStr   := Caption + ': ';
   _Idle := false;

   litk          := GetLastInputTick;
   tk            := GetTickCount;

   ActiveTime    := 0;
   ActiveTime_   := 0;
   InactiveTime  := 0;
   InactiveTime_ := 0;
   LastState     := GetIdleState;
   LastState_    := LastState;
   lt      := tk;
   lt_     := tk;

   StartDate := Now;

   ShowInfoExecute(Sender);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var fn: string;
    fh: integer;
    buf: string;
const ln = #13#10;
begin
  UAC.Update;
  
  OnStateChangeExecute(Sender);
  OnStateChange_Execute(Sender);
  if (ActiveTime < 1000) and (ActiveTime_ < 1000) then Exit;
  fn := ChangeFileExt(Application.ExeName, '.log');
  fh := FileOpen(fn, fmOpenReadWrite or fmShareDenyNone);
  if fh <= 0 then fh := FileCreate(fn);
  if fh <= 0 then Exit;
  FileSeek(fh, 0, 2);
  buf := ' - - -' + ln +
         '| ' + DateTimeToStr(StartDate) + ' |' + ln +
         'Present: ' + MSec2StrTime(ActiveTime_ ) + ln +
         'Absent : ' + MSec2StrTime(InactiveTime_ ) + ln +
         'Busy   : ' + MSec2StrTime(UAC.BusyTime) + ln +
         'Total  : ' + MSec2StrTime(UAC.TotalTime) + ln +
         '| ' + DateTimeToStr(Now) + ' |' + ln ;

  FileWrite(fh, PAnsiChar(buf)^, length(buf));

  FileClose(fh);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i:dword;
    schg: boolean;
begin
   schg := UAC.Update;
   
   litk := GetLastInputTick;
   tk   := GetTickCount;

   schg := LastInputStateChanged(LastState, lt, UAInactiveTimeout);
   if schg then OnStateChangeExecute(Sender);

   if not _Idle and LastInputStateChanged(LastState_, lt_, IdleTimeout) then
     OnStateChange_Execute(Sender);

   if schg or boolean(UAC.Busy) or ((tk shr 5) and 2 = 0) then
      ShowInfoExecute(Sender);

   // Timer adjustment
   if Sender is TTimer then begin
      i := UAInactiveTimeout shr 1;
      if i = 0 then inc(i);
      (Sender as TTimer).Interval := i;
   end;
   
   if _toHide then begin
      _toHide := false;
      Hide;      
   end;
end;

procedure TForm1.OnStateChangeExecute(Sender: TObject);
var i: DWord;
begin
      if LastState then begin // -> active
        inc(InactiveTime, litk - lt);
        inc(ActiveTime, tk - litk);
        if not _Idle then Self.Color := clSkyBlue;
      end else begin          // -> idle
        if lt < litk then i := litk else i := lt;
        inc(ActiveTime, i - lt);
        inc(InactiveTime, tk - i);
        if not _Idle then Self.Color := clMoneyGreen;
      end;
      lt := tk;
      LastState := not LastState;
end;

procedure TForm1.OnStateChange_Execute(Sender: TObject);
var i: DWord;
begin
      if _Idle and LastState_ then Exit;

      if LastState_ then begin // -> active
        inc(InactiveTime_, litk - lt_);
        inc(ActiveTime_, tk - litk);
      end else begin          // -> idle
        if lt_ < litk then i := litk else i := lt_;
        inc(ActiveTime_, i - lt_);
        inc(InactiveTime_, tk - i);
        Self.Color := clYellow;
      end;
      lt_ := tk;
      LastState_ := not LastState_;
end;

procedure TForm1.ShowInfoExecute(Sender: TObject);
var statestr: string;
    i: DWORD;
begin
{ Old 
    if lt < litk then i := litk else i := lt;
    LIt.Caption    := MSec2StrTime((InactiveTime+tk-i) );
    LAt.Caption    := MSec2StrTime((ActiveTime+i-lt) );
    Label3.Caption := MSec2StrTime((InactiveTime+ActiveTime+tk-lt) );
}
    LIt.Caption    := MSec2StrTime(UAC.IdleTime);
    LAt.Caption    := MSec2StrTime(UAC.BusyTime);
    Label3.Caption := MSec2StrTime(UAC.TotalTime);

{
    if lt_ < litk then i := litk else i := lt_;
    LIt_.Caption   := MSec2StrTime((InactiveTime_+tk-i) );
    LAt_.Caption   := MSec2StrTime((ActiveTime_+i-lt_) );
    Label4.Caption := MSec2StrTime((InactiveTime_+ActiveTime_+tk-lt_) );
}

    LIt_.Caption   := MSec2StrTime(UAC.AbsentTime);
    LAt_.Caption   := MSec2StrTime(UAC.PresentTime);
    Label4.Caption := IntToStr(UAC.TotalTime);

   i := tk - lt;
   if LastState then statestr := 'Idle' else statestr := 'Active';
   Caption := Format(statestr + ': %0.3f :: ' + FormCaptStr, [i/1000]) ;

   i := tk - lt_;
   if LastState_ then begin
      statestr := 'Absent';
      StatusBar1.Panels[2].Text := 'A:'+MSec2StrTime(i);
   end else begin
      if LastState then statestr := 'Idle'
                   else statestr := 'Busy';
      StatusBar1.Panels[1].Text := 'P:'+MSec2StrTime(i);
   end;
   StatusBar1.Panels[0].Text := statestr;
end;

procedure TForm1.EITOChange(Sender: TObject);
var t: string;
begin
   t := (Sender as TCustomEdit).Text;
   if t = '' then t := '0';
   IdleTimeout := StrToInt(t)*1000;
   UAC.AbsentTimeout := IdleTimeout;
end;

procedure TForm1.SetIdleExecute(Sender: TObject);
begin
  OnStateChangeExecute(Sender);
  OnStateChange_Execute(Sender);

   _Idle := not _Idle;
   if _Idle then begin
      BSetI.Caption := 'Set Active';
      Self.Color := clYellow;
   end else begin
      BSetI.Caption := 'Set Idle';
      Self.Color := clSkyBlue;
   end;
end;

procedure TForm1.TrayMessage(var Msg: TMessage);
begin
  case Msg.lParam of
    WM_LBUTTONDOWN:
    begin
      ShowFromTrayExecute(Self);
    end;
    WM_RBUTTONDOWN:
    begin
      HideToTrayExecute(Self);
    end;
    WM_MOUSEMOVE:
    begin
      Self.BringToFront;
    end;
    else StatusBar1.Panels[2].Text := IntToStr(Msg.lParam) + ':' + IntToStr(Msg.WParam);
  end;
end;

procedure TForm1.WMSize(var Msg: TWMSize);
begin

end;

procedure TForm1.HideToTrayExecute(Sender: TObject);
begin
   Hide;
end;

procedure TForm1.ShowFromTrayExecute(Sender: TObject);
begin
   Application.Restore;
   Show;
   WindowState := wsNormal;   
end;

procedure TForm1.TrayMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      ShowFromTrayExecute(Self);
    end;
    mbRight:
    begin
      HideToTrayExecute(Self);
    end;
    mbMiddle:
    begin
      Self.BringToFront;
    end;
  end;
end;

procedure TForm1.StatusBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
    s: string;
begin
  i := OnWhichPanel(Sender as TStatusBar, x);
  s := 'http://' + TStatusBar(Sender).Panels[i].Text;
  case i of
    0:;
    1:;
    2:;
    3: ShellExecute(Application.Handle, 'open', PChar(s), nil, nil, 1);
  end;
end;

procedure TForm1.StatusBar1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var i: integer;
begin
  i := OnWhichPanel(Sender as TStatusBar, x);
  case i of
    0: TStatusBar(Sender).Hint := TStatusBar(Sender).Panels[i].Text;
    1: TStatusBar(Sender).Hint := 'Last active session lease time';
    2: TStatusBar(Sender).Hint := 'Last idle session lease time';
    3: TStatusBar(Sender).Hint := 'Author';
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   UAC.Free;
end;

end.
