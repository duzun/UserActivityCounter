unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Registry,
  UserAcReg, StdCtrls, ActnList, ComCtrls, ShellApi, TrayIcon;

const WM_ICONTRAY = WM_USER + 1;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LBusy: TLabel;
    LIdle: TLabel;
    ActionList1: TActionList;
    ShowInfo: TAction;
    Label1: TLabel;
    Label2: TLabel;
    LITO: TLabel;
    LPresent: TLabel;
    LAbsent: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OnAbsentChange: TAction;
    EITO: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    OnBusyChange: TAction;
    SetIdle: TAction;
    HideToTray: TAction;
    ShowFromTray: TAction;
    OnPresentChange: TAction;
    OnIdleChange: TAction;
    OnPresenceChange: TAction;
    LBusyL: TLabel;
    LIdleL: TLabel;
    lPresentL: TLabel;
    LAbsentL: TLabel;
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
    procedure StatusBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdleChangeExecute(Sender: TObject);
    procedure OnPresenceChangeExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure OnPresentChangeExecute(Sender: TObject);
  protected
    procedure TrayMessage(var Msg: TMessage); message WM_ICONTRAY;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  private
    { Private declarations }
    TrayIcon: TTrayIcon;
    _csv_buf: string;
    _csv_fn: TFileName;
    usrnm: string;
    FormCaptStr: String;
    _Idle: Boolean;
    StartDate: TDateTime;

    _toHide: boolean;
    
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

function MSec2StrTime(msec:ULong):string;
var d: TDateTime;
begin
   d := msec / MSecsPerDay;
   Result := '';
   if d >= 1 then Result := IntToStr(Floor(d)) + '-';
   Result := Result + TimeToStr(d, FormatSettings);
end;

function FileAppend(fn:string; const buf:string): integer;
var fh: integer;
begin
  Result := -1;
  fh := FileOpen(fn, fmOpenReadWrite or fmShareDenyNone);
  if fh <= 0 then fh := FileCreate(fn);
  if fh <= 0 then Exit;
  FileSeek(fh, 0, 2);
  Result := FileWrite(fh, PAnsiChar(buf)^, length(buf));
  FileClose(fh);
end;

function Report(fn:string; var buf: string; s: integer; t:DWord; a:DWord=0; tp:DWord=0; ta:DWord=0): boolean;
var p: integer;
    dt, tm: string;
const ln = #13#10;

begin
  Result := false;
  
  // date ; came ; left ; present time ; busy time ; total present ; total absent
  
  tm := DateTimeToStr(Now);
  p := pos(' ', tm);
  dt := copy(tm, 1, p-1);
  tm := copy(tm, p+1, length(tm));
  
  if s = 1 then begin // came
     buf := buf + ln + dt + ';' + tm + ';';
  end else
  if s <= 0 then begin // left
     buf := buf + tm + ';' + IntToStr(round(t/1000)) + ';' + IntToStr(round(a/1000)) + ';';
     if s = -1 then begin // quit
        buf := buf + IntToStr(round(tp/1000)) + ';' + IntToStr(round(ta/1000)) + ';' + ln;
     end;
  end;

  if buf = '' then Exit;
  
  Result := FileAppend(fn, buf) >= 0;
  
  if Result then buf := '';
end;

procedure LoadCSVFile (FileName: String; separator: char);
var f: TextFile;
    s1, s2: string;
    i, j: integer;
begin
     i := 0;
     AssignFile (f, FileName);
     Reset(f);
     while not eof(f) do
      begin
           readln (f, s1);
           i := i + 1;
           j := 0;
           while pos(separator, s1)<>0 do
            begin
                 s2 := copy(s1,1,pos(separator, s1)-1);
                 j := j + 1;
                 Delete(s1, 1, pos(separator, S1));
//                 StringGrid1.Cells[j-1, i-1] := s2;
            end;
           if pos (separator, s1)=0 then
            begin
                 j := j + 1;
//                 StringGrid1.Cells[j-1, i-1] := s1;
            end;
//           StringGrid1.ColCount := j;
//           StringGRid1.RowCount := i+1;
      end;
     CloseFile(f);
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
  TrayIcon.Hint := Caption;
  TrayIcon.OnMouseDown := TrayMouseDown;

   usrnm := GetEnvironmentVariable('USERNAME');
   _csv_buf := '';
   _csv_fn := Application.ExeName;
   _csv_fn := ExtractFilePath(_csv_fn) + usrnm + '_' + ChangeFileExt(ExtractFileName(_csv_fn), '.csv');
   if not FileExists(_csv_fn) then FileAppend(_csv_fn, 'date;came;left;pres.;busy;t.pres.;t.busy');

   _toHide := ParamStr(1) = '/min';

   UAC := TUserActivityCounter.Create;
   UAC.AbsentTimeout := 300;
   UAC.OnBusy := OnBusyChangeExecute;
   UAC.OnIdle := OnIdleChangeExecute;
   UAC.OnPresent := OnPresentChangeExecute;
   UAC.OnAbsent  := OnAbsentChangeExecute;
   UAC.OnPresentChange := OnPresenceChangeExecute;

   StartDate := Now;

   EITOChange(EITO);
   FormCaptStr   := Caption + ': ';
   _Idle := false;

   ShowInfoExecute(Sender);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var fn: string;
const ln = #13#10;
begin
  UAC.Update;

  Report(_csv_fn, _csv_buf, -1, UAC.PresentTimeLast, UAC.BusyTimeLast, UAC.PresentTime, UAC.BusyTime);  // Quit

  if (UAC.BusyTime < 1000) then Exit;

  fn := ChangeFileExt(Application.ExeName, '.log');

  FileAppend(fn, 
         ' - - -' + ln +
         '| ' + DateTimeToStr(StartDate) + ' |' + ln +
         'Present: ' + MSec2StrTime(UAC.PresentTime) + ln +
         'Absent : ' + MSec2StrTime(UAC.AbsentTime ) + ln +
         'Busy   : ' + MSec2StrTime(UAC.BusyTime) + ln +
         'Total  : ' + MSec2StrTime(UAC.TotalTime) + ln +
         '| ' + DateTimeToStr(Now) + ' |' + ln   
  );

end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(UAC)
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   case Key of
   27: Self.Close;
   32: if ssCtrl in Shift then begin
          EITO.Visible := not EITO.Visible;
       end;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i:dword;
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

   if _toHide then begin
      _toHide := false;
      Hide;
   end;
end;

procedure TForm1.OnBusyChangeExecute(Sender: TObject);
var i: DWord;
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
    LBusy.Caption      := MSec2StrTime(UAC.BusyTime);
    LIdle.Caption      := MSec2StrTime(UAC.IdleTime);
    LPresent.Caption   := MSec2StrTime(UAC.PresentTime);
    LAbsent.Caption    := MSec2StrTime(UAC.AbsentTime);

    LBusyL.Caption      := MSec2StrTime(UAC.BusyTimeLast(true));
    LIdleL.Caption      := MSec2StrTime(UAC.IdleTimeLast);
    LPresentL.Caption   := MSec2StrTime(UAC.PresentTimeLast(true));
    LAbsentL.Caption    := MSec2StrTime(UAC.AbsentTimeLast);

   if not UAC.Present then begin
      statestr := 'Absent';
      StatusBar1.Panels[2].Text := 'A:'+MSec2StrTime(UAC.AbsentTimeLast);
   end else begin
      if UAC.Busy then statestr := 'Busy'
                  else statestr := 'Idle';
      StatusBar1.Panels[1].Text := 'P:'+MSec2StrTime(UAC.PresentTimeLast);
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

procedure TForm1.TrayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    3: ShellExecute(Application.Handle, 'open', PAnsiChar(s), nil, nil, 1);
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
    if UAC.Present then Report(_csv_fn, _csv_buf, 1, UAC.AbsentTimeLast)
                   else Report(_csv_fn, _csv_buf, 0, UAC.PresentTimeLast, UAC.BusyTimeLast);
end;


procedure TForm1.FormClick(Sender: TObject);
begin
   EITO.Visible := false;
end;

procedure TForm1.OnPresentChangeExecute(Sender: TObject);
begin
  if true then ;
end;

end.
