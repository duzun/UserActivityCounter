unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  UserAcReg, StdCtrls, ActnList, Graph, ComCtrls, ShellApi;

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
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ShowInfoExecute(Sender: TObject);
    procedure EITOChange(Sender: TObject);
    procedure OnStateChange_Execute(Sender: TObject);
    procedure OnStateChangeExecute(Sender: TObject);
    procedure SetIdleExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure TrayMessage(var Msg: TMessage); message WM_ICONTRAY;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  private
    { Private declarations }
    TrayIconData: TNotifyIconData;
        
    FormCaptStr: String;
    tk: DWord;   // TickCount
    litk: DWord; // LastInputTickCount
    lt, lt_: DWord; // LastTick
    _Idle: Boolean;
    StartDate: TDateTime;
  public
    { Public declarations }
    InactiveTime: ULong;
    InactiveTime_: ULong;
    ActiveTime: ULong;
    ActiveTime_: ULong;
    IdleTimeout: DWord;
    LastState, LastState_: Boolean; // true - active, false - idle
  end;

var
  Form1: TForm1;

implementation

uses DateUtils;

{$R *.dfm}

function MSec2StrTime(msec:ULong):string;
var d: TDateTime;
begin
   d := msec / MSecsPerDay;
   if d >= 1 then Result := DateTimeToStr(d)
             else Result := TimeToStr(d);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
     #27: Self.Close;
   end;  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with TrayIconData do
  begin
    cbSize := SizeOf(TrayIconData);
    Wnd := Handle;
    uID := 0;
    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    uCallbackMessage := WM_ICONTRAY;
    hIcon := Application.Icon.Handle;
    StrPCopy(szTip, Self.Caption);
  end;
  
   IdleTimeout   := 5*60*1000; // 5 min.
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

   EITO.Text := IntToStr(IdleTimeout div 1000);
   StartDate := Now;

   ShowInfoExecute(Sender);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i:dword;
    schg: boolean;
begin
   litk := GetLastInputTick;
   tk   := GetTickCount;

   schg := LastInputStateChanged(LastState, lt, UAInactiveTimeout);
   if schg then OnStateChangeExecute(Sender);

   if LastInputStateChanged(LastState_, lt_, IdleTimeout) then
     OnStateChange_Execute(Sender);

   if schg or not LastState or ((tk shr 5) and 2 = 0) then
      ShowInfoExecute(Sender);

   // Timer adjustment
   if Sender is TTimer then begin
      i := UAInactiveTimeout shr 1;
      if i = 0 then inc(i);
      (Sender as TTimer).Interval := i;
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
    if lt < litk then i := litk else i := lt;
    LIt.Caption := MSec2StrTime((InactiveTime+tk-i) );
    LAt.Caption := MSec2StrTime((ActiveTime+i-lt) );
    Label3.Caption := MSec2StrTime((InactiveTime+ActiveTime+tk-lt) );
    if lt_ < litk then i := litk else i := lt_;
    LIt_.Caption := MSec2StrTime((InactiveTime_+tk-i) );
    LAt_.Caption := MSec2StrTime((ActiveTime_+i-lt_) );
    Label4.Caption := MSec2StrTime((InactiveTime_+ActiveTime_+tk-lt_) );

   i := tk - lt;
   if LastState then statestr := 'Idle' else statestr := 'Active';
   Caption := Format(statestr + ': %0.3f :: ' + FormCaptStr, [i/1000]) ;

   i := tk - lt_;
   if LastState_ then statestr := 'Idle' else statestr := 'Active';
   StatusBar1.Panels[0].Text := statestr;
   StatusBar1.Panels[1].Text := TimeToStr(i / MSecsPerDay);
end;

procedure TForm1.EITOChange(Sender: TObject);
var t: string;
begin
   t := (Sender as TCustomEdit).Text;
   if t = '' then t := '0';
   IdleTimeout := StrToInt(t)*1000;
end;

procedure TForm1.SetIdleExecute(Sender: TObject);
begin
   _Idle := not _Idle;
   if _Idle then begin
      BSetI.Caption := 'Set Active';
      Self.Color := clYellow;
   end else begin
      BSetI.Caption := 'Set Idle';
      Self.Color := clSkyBlue;
   end;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
   Shell_NotifyIcon(NIM_DELETE, @TrayIconData);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var fn: string;
    fh: integer;
    buf: string;
const ln = #13#10;
begin
  OnStateChangeExecute(Sender);
  OnStateChange_Execute(Sender);
  if (ActiveTime < 1000) and (ActiveTime_ < 1000) then Exit;
  fn := ChangeFileExt(Application.ExeName, '.log');
  fh := FileOpen(fn, fmOpenReadWrite or fmShareDenyNone);
  if fh <= 0 then fh := FileCreate(fn);
  if fh <= 0 then Exit;
  FileSeek(fh, 0, 2);
  buf := '';
  buf := '~ ' + DateTimeToStr(StartDate) + ln +
         'Idle time  : ' + MSec2StrTime(InactiveTime_ ) + ln +
         'Active time: ' + MSec2StrTime(ActiveTime_ ) + ln +
         'Real Idle  : ' + MSec2StrTime(InactiveTime ) + ln +
         'Real Active: ' + MSec2StrTime(ActiveTime ) + ln +
         'Total:       ' + MSec2StrTime(InactiveTime_+ActiveTime_ ) + ln +
         '~ ' + DateTimeToStr(Now) + ln +
         ' * * * ' + ln;

  FileWrite(fh, PAnsiChar(buf)^, length(buf));

  FileClose(fh);
end;


procedure TForm1.TrayMessage(var Msg: TMessage);
begin
  case Msg.lParam of
    WM_LBUTTONDOWN:
    begin
      Self.Show;
    end;
    WM_RBUTTONDOWN:
    begin
      Self.Hide;
    end;
  end;
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
   Self.Hide;
end;

procedure TForm1.WMSize(var Msg: TWMSize);
begin

end;

procedure TForm1.FormHide(Sender: TObject);
begin
  Shell_NotifyIcon(NIM_ADD, @TrayIconData);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Shell_NotifyIcon(NIM_DELETE, @TrayIconData);
end;

end.
