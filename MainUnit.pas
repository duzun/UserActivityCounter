unit MainUnit;

{
    @author  Dumitru Uzun (DUzun.Me)
    @version 1.0.1
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Registry,
  UserAcReg, StdCtrls, ActnList, ComCtrls, ShellApi, TrayIcon;

const csv_cell_sep = #09';';
      csv_ln_sep   = #13#10;

type
  TForm1 = class(TForm)
    Timer1          : TTimer;
    LBusy           : TLabel;
    LIdle           : TLabel;
    ActionList1     : TActionList;
    ShowInfo        : TAction;
    Label1          : TLabel;
    Label2          : TLabel;
    LPresent        : TLabel;
    LAbsent         : TLabel;
    Label5          : TLabel;
    Label6          : TLabel;
    OnAbsentChange  : TAction;
    EITO            : TEdit;
    Label3          : TLabel;
    Label4          : TLabel;
    StatusBar1      : TStatusBar;
    OnBusyChange    : TAction;
    SetIdle         : TAction;
    HideToTray      : TAction;
    ShowFromTray    : TAction;
    OnPresentChange : TAction;
    OnIdleChange    : TAction;
    OnPresenceChange: TAction;
    LBusyL          : TLabel;
    LIdleL          : TLabel;
    lPresentL       : TLabel;
    LAbsentL        : TLabel;
    LDateTime       : TLabel;
    Label7          : TLabel;
    PauseBtn: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowInfoExecute(Sender: TObject);
    procedure EITOChange(Sender: TObject);
    procedure OnAbsentChangeExecute(Sender: TObject);
    procedure OnBusyChangeExecute(Sender: TObject);
    procedure SetIdleExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HideToTrayExecute(Sender: TObject);
    procedure ShowFromTrayExecute(Sender: TObject);
    procedure TrayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TrayMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdleChangeExecute(Sender: TObject);
    procedure OnPresenceChangeExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure OnPresentChangeExecute(Sender: TObject);
    procedure PauseBtnClick(Sender: TObject);
  protected
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Message: TWMEndSession); message WM_ENDSESSION;
    procedure WMPowerBroadcast(var Message: TMessage); message WM_POWERBROADCAST;

  private
    { Private declarations }
    TrayIcon: TTrayIcon;
    _csv_buf: string;
    _csv_fn: TFileName;
    usrnm: string;
    FormCaptStr: String;
    _Idle: Boolean;
    StartDate: TDateTime;

    _toHide: byte;

  public
    { Public declarations }
    IdleTimeout: DWord;

    UAC: TUserActivityCounter;
  end;

var
  Form1: TForm1;


implementation

uses DateUtils, Math;

{$R *.dfm}


function FileAppend(fn:string; const buf:string): integer;
var fh: integer;
begin
  Result := -1;
  fh := FileOpen(fn, fmOpenReadWrite or fmShareDenyNone);
  if fh <= 0 then fh := FileCreate(fn);
  if fh <= 0 then Exit;
  FileSeek(fh, 0, 2);
  Result := FileWrite(fh, PAnsiChar(UTF8String(buf))^, length(buf));
  FileClose(fh);
end;

type TUAC_Report = (ur_none, ur_came, ur_left, ur_last);
var last_report: Tuac_report;

function Report(fn:string; var buf: string; uac_report: TUAC_Report; UAC: TUserActivityCounter): boolean;
      var dt, tm: string;

      function mktm(n:TDateTime): string;
      var p: integer;
      begin
          Result := DateTimeToStr(n, FormatSettings);
          p  := pos(' ', Result);
          dt := copy(Result, 1, p-1);
          Result := copy(Result, p+1, length(Result));
          tm := Result;
      end;

      function add_cell(s:string): string; overload; begin Result := s + csv_cell_sep; buf := buf + Result; end;
      function add_cell(w:DWord): string; overload; begin Result := add_cell(MSec2StrTime(w)) end;
begin
  Result := true;
  UAC.Update;

  // date ; came ; left ; present ; absent ; total present ; total absent ; busy ; down

  // UAC state changed
  if uac_report <> last_report then begin
      // came
      if uac_report = ur_came then begin
         if last_report = ur_left then begin
            add_cell(UAC.AbsentTimeLast);
            buf := buf + csv_cell_sep + csv_cell_sep + csv_cell_sep + csv_cell_sep;
         end;
         buf := buf + csv_ln_sep;
         // mktm(Now);
         mktm(Now - UAC.PresentTimeLast / MSecsPerDay);
         add_cell(dt);
         add_cell(tm);
      end else
      // left
      if uac_report >= ur_left then begin
         if last_report < ur_left then begin
            mktm(Now - UAC.IdleTimeLast / MSecsPerDay);
            add_cell(tm);
            add_cell(UAC.PresentTimeLast);
         end;
         // quit
         if uac_report = ur_last then begin
            mktm(Now);
            add_cell(UAC.AbsentTimeLast);
            add_cell(UAC.PresentTime);
            add_cell(UAC.AbsentTime);
            add_cell(UAC.BusyTime);
            add_cell(tm);
            buf := buf + csv_ln_sep;
         end;
      end;
      last_report := uac_report;
  end;

  if buf = '' then Exit;

  Result := FileAppend(fn, buf) >= 0;

  if Result then buf := '';
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrayIcon := TTrayIcon.Create(Self);
  TrayIcon.Hint        := Caption;
  TrayIcon.OnMouseDown := TrayMouseDown;
  TrayIcon.OnMouseMove := TrayMouseMove;

   usrnm := GetEnvironmentVariable('USERNAME');
   _csv_buf := '';
   _csv_fn := Application.ExeName;
   _csv_fn := ExtractFilePath(_csv_fn) + usrnm + '_' + ChangeFileExt(ExtractFileName(_csv_fn), '.csv');
   if not FileExists(_csv_fn) then
    FileAppend(_csv_fn,
       '~ date ~ ' + csv_cell_sep +
       '~ came ~ ' + csv_cell_sep + '~ left ~ ' + csv_cell_sep +
       '~ pres.~ ' + csv_cell_sep + '~ abs. ~ ' + csv_cell_sep +
       '~t.pres~ ' + csv_cell_sep + '~t.abs.~ ' + csv_cell_sep +
       '~ busy ~ ' + csv_cell_sep + '~ down ~ ' + csv_cell_sep
    );

   _toHide := byte(ParamStr(1) = '/min');

   UAC := TUserActivityCounter.Create;
   UAC.AbsentTimeout   := 300;
   UAC.OnBusy          := OnBusyChangeExecute;
   UAC.OnIdle          := OnIdleChangeExecute;
   UAC.OnPresent       := OnPresentChangeExecute;
   UAC.OnAbsent        := OnAbsentChangeExecute;
   UAC.OnPresentChange := OnPresenceChangeExecute;

   UAC.Update;
   if UAC.Present then UAC.PresentChanged(Self);
   if UAC.Busy    then UAC.BusyChanged(Self);

   StartDate := Now;

   EITOChange(EITO);
   FormCaptStr   := Caption + ': ';
   _Idle := false;

   ShowInfoExecute(Sender);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (_toHide < 2) or not Report(_csv_fn, _csv_buf, ur_last, UAC) then begin
     Action := caNone;
     Self.Hide;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var fn: string;
begin
  Report(_csv_fn, _csv_buf, ur_last, UAC);  // Quit

  if (UAC.BusyTime >= 1000) then begin
    fn := ChangeFileExt(Application.ExeName, '.log');
    FileAppend(fn,
         ' - - -' + csv_ln_sep +
         '| ' + DateTimeToStr(StartDate, FormatSettings) + ' |' + csv_ln_sep +
         'Present: ' + UAC.PresentTimeStr + csv_ln_sep +
         'Absent : ' + UAC.AbsentTimeStr  + csv_ln_sep +
         'Busy   : ' + UAC.BusyTimeStr    + csv_ln_sep +
         'Total  : ' + UAC.TotalTimeStr   + csv_ln_sep +
         '| ' + DateTimeToStr(Now, FormatSettings) + ' |' + csv_ln_sep
    );
  end;

  FreeAndNil(UAC)
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const exit_condition: TShiftState = [ssAlt,ssCtrl];
begin
   case Key of
   27: begin
       if exit_condition = Shift then _toHide := 2;
       Self.Hide;
   end;
   32: if ssCtrl in Shift then begin
          EITO.Visible := not EITO.Visible;
       end;
   end;
end;

procedure TForm1.TrayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const exit_condition: TShiftState = [ssAlt,ssCtrl,ssRight];
begin
  if exit_condition = Shift then _toHide := 2;
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
          BringWindowToTop(Self.Handle);
          BringWindowToTop(Application.Handle);
        end;
  end;
end;

procedure TForm1.TrayMouseMove;
begin
  TrayIcon.Hint := 'UAC: ' + UAC.PresentTimeStr;
end;

procedure TForm1.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
    _toHide := 2;
    if Report(_csv_fn, _csv_buf, ur_last, UAC) then
        Message.Result := 1
    else
        Message.Result := 0;

    inherited;
end;

procedure TForm1.WMEndSession(var Message: TWMEndSession);
begin
   if Message.EndSession then _toHide := 2;
   inherited;
end;

procedure TForm1.WMPowerBroadcast;
const  // http://winapi.freetechsecrets.com/win32/WIN32WMPOWERBROADCAST.htm
     PBT_APMBATTERYLOW         = $09;
     PBT_APMOEMEVENT           = $0B;
     PBT_APMPOWERSTATUSCHANGE  = $0A;
     PBT_APMQUERYSUSPEND       = $00; // win ask the app if it is ok to suspend
     PBT_APMQUERYSUSPENDFAILED = $02; // some app has not agree to suspend
     PBT_APMRESUMECRITICAL     = $06; // resume after a critical suspension
     PBT_APMRESUMESUSPEND      = $07; // resume after a suspension
     PBT_APMSUSPEND            = $04; // system is about to suspend. save work
begin
   case Message.WParam of
        PBT_APMBATTERYLOW         : ;
        PBT_APMOEMEVENT           : ;
        PBT_APMPOWERSTATUSCHANGE  : ;
        PBT_APMQUERYSUSPEND       : ;
        PBT_APMQUERYSUSPENDFAILED : ;
        PBT_APMRESUMESUSPEND      : begin
            UAC.Reset;
            UAC.Update;
            if UAC.Present then UAC.PresentChanged(Self);
            if UAC.Busy    then UAC.BusyChanged(Self);
        end;
        PBT_APMRESUMECRITICAL     : begin
            Report(_csv_fn, _csv_buf, ur_last, UAC);
            UAC.Reset;
            UAC.Update;
            if UAC.Present then UAC.PresentChanged(Self);
            if UAC.Busy    then UAC.BusyChanged(Self);
        end;
        PBT_APMSUSPEND            : begin
            Report(_csv_fn, _csv_buf, ur_last, UAC);
            UAC.Reset;
            UAC.Update;
            if UAC.Present then UAC.PresentChanged(Self);
            if UAC.Busy    then UAC.BusyChanged(Self);
        end;
   end;
   inherited;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i: dword;
    schg: boolean;
begin
   if UAC = nil then Exit;

   schg := UAC.Update;

   if schg or UAC.Busy or ((GetTickCount shr 5) and 2 = 0) then
      ShowInfoExecute(Sender);

   // Timer adjustment
   if Sender is TTimer then begin
      i := UAC.IdleTimeout shr 1;
      if i = 0 then inc(i);
      (Sender as TTimer).Interval := i;
   end;

   case _toHide of
   1: begin
      _toHide := 0;
      Self.Hide;
      end;
   2: begin
      if Report(_csv_fn, _csv_buf, ur_last, UAC) then
         Self.Close;
      end;
   end;
end;

procedure TForm1.OnBusyChangeExecute(Sender: TObject);
begin
   Self.Color := clSkyBlue;
end;

procedure TForm1.OnIdleChangeExecute(Sender: TObject);
begin
    Self.Color := clMoneyGreen;
end;

procedure TForm1.OnAbsentChangeExecute(Sender: TObject);
begin
    Self.Color := clYellow;
end;

procedure TForm1.ShowInfoExecute(Sender: TObject);
var statestr: string;
    i: DWORD;
begin
    LBusy.Caption     := UAC.BusyTimeStr;
    LIdle.Caption     := UAC.IdleTimeStr;
    LPresent.Caption  := UAC.PresentTimeStr;
    LAbsent.Caption   := UAC.AbsentTimeStr;

    LBusyL.Caption    := UAC.BusyTimeLastStr(true);
    LIdleL.Caption    := UAC.IdleTimeLastStr;
    LPresentL.Caption := UAC.PresentTimeLastStr(true);
    LAbsentL.Caption  := UAC.AbsentTimeLastStr;

    LDateTime.Caption := DateTimeToStr(Now, FormatSettings);

   if not UAC.Present then begin
      statestr := 'Absent';
      StatusBar1.Panels[2].Text := 'A:'+UAC.AbsentTimeLastStr;
   end else begin
      if UAC.Busy then statestr := 'Busy'
                  else statestr := 'Idle';
      StatusBar1.Panels[1].Text := 'P:'+UAC.PresentTimeLastStr;
   end;
   StatusBar1.Panels[0].Text := statestr;

   if UAC.Busy then i := UAC.BusyTimeLast else i := UAC.IdleTimeLast;
   Caption := Format(statestr + ': %0.3f :: ' + FormCaptStr, [i/1000]) ;

end;

procedure TForm1.EITOChange(Sender: TObject);
var t: string;
begin
   t := Trim((Sender as TCustomEdit).Text);
   if t = '' then t := '0';
   IdleTimeout := StrToInt(t)*1000;
   UAC.AbsentTimeout := IdleTimeout;
end;

procedure TForm1.SetIdleExecute(Sender: TObject);
begin
   UAC.Update;
   _Idle := not _Idle;
   if _Idle then begin
      Self.Color := clYellow;
   end else begin
      Self.Color := clSkyBlue;
   end;
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

procedure TForm1.StatusBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
    s: string;
begin
  i := OnWhichPanel(Sender as TStatusBar, x);
  s := 'http://' + TStatusBar(Sender).Panels[i].Text;
  case i of
    0:;
    1:;
    2:;
    3: ShellExecute(Application.Handle, 'open', LPCTSTR(s), nil, nil, 1);
  end;
end;

procedure TForm1.StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TForm1.OnPresenceChangeExecute(Sender: TObject);
begin
    if UAC.Present then Report(_csv_fn, _csv_buf, ur_came, UAC)
                   else Report(_csv_fn, _csv_buf, ur_left, UAC);
end;


procedure TForm1.FormClick(Sender: TObject);
begin
   EITO.Hide;
end;

procedure TForm1.OnPresentChangeExecute(Sender: TObject);
begin
  if true then ;
end;

procedure TForm1.PauseBtnClick(Sender: TObject);
begin
  UAC.Paused := not UAC.Paused;
  if UAC.Paused then begin
    PauseBtn.Caption := 'Resume';
  end else begin
    PauseBtn.Caption := 'Pause';
  end;
end;

initialization
   last_report := ur_none;

end.
