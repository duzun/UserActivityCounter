unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  UserAcReg, StdCtrls, ActnList, Graph;

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
    StateChange_: TAction;
    EITO: TEdit;
    Label7: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ShowInfoExecute(Sender: TObject);
    procedure EITOChange(Sender: TObject);
  private
    { Private declarations }
    FormCaptStr: String;
    tk: DWord;   // TickCount
    litk: DWord; // LastInputTickCount
    lt, lt_: DWord; // LastTick
    k: DWord;
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

{$R *.dfm}

function MSec2StrTime(var msec:ULong):string;
var h,m,s,ms:word;
begin
//    DecodeTime(msec / MSecsPerDay, h,m,s,ms);
    Result := TimeToStr(msec / MSecsPerDay);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
     #27: Self.Close;
   end;  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   IdleTimeout   := 5*60*1000; // 5 min.
   FormCaptStr   := Caption + ': ';

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

   EITO.Text     := IntToStr(IdleTimeout div 1000);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i, j:dword;
    schg: boolean;
begin
   litk := GetLastInputTick;
   tk   := GetTickCount;

   schg := LastInputStateChanged(LastState, lt, UAInactiveTimeout);
   if schg then begin
      if LastState then begin // -> active
        inc(InactiveTime, litk - lt);
        inc(ActiveTime, tk - litk);
        Self.Color := clSkyBlue;
      end else begin          // -> idle
        if lt < litk then i := litk else i := lt;
        inc(ActiveTime, i - lt);
        inc(InactiveTime, tk - i);
        Self.Color := clMoneyGreen;
      end;
     LastState := not LastState;
     lt := tk;
   end;

   schg := LastInputStateChanged(LastState_, lt_, IdleTimeout);
   if schg then begin
      if LastState_ then begin // -> active
        inc(InactiveTime_, litk - lt_);
        inc(ActiveTime_, tk - litk);
      end else begin          // -> idle
        if lt_ < litk then i := litk else i := lt_;
        inc(ActiveTime_, i - lt_);
        inc(InactiveTime_, tk - i);
        Self.Color := clYellow;
      end;
     LastState_ := not LastState_;
     lt_ := tk;
   end;

   if (i > 1000) or not LastState or (tk = lt) then
      ShowInfoExecute(Sender);

   // Timer adjustment
   if Sender is TTimer then begin
      i := UAInactiveTimeout shr 1;
      if i = 0 then inc(i);
      (Sender as TTimer).Interval := i;
   end;
end;

procedure TForm1.ShowInfoExecute(Sender: TObject);
var statestr: string;
    i: DWORD;
begin
    if lt < litk then i := litk else i := lt;
    LIt.Caption := TimeToStr((InactiveTime+tk-i) / MSecsPerDay);
    LAt.Caption := TimeToStr((ActiveTime+i-lt) / MSecsPerDay);
    Label3.Caption := TimeToStr((InactiveTime+ActiveTime+tk-lt) / MSecsPerDay);
    if lt_ < litk then i := litk else i := lt_;
    LIt_.Caption := TimeToStr((InactiveTime_+tk-i) / MSecsPerDay);
    LAt_.Caption := TimeToStr((ActiveTime_+i-lt_) / MSecsPerDay);
    Label4.Caption := TimeToStr((InactiveTime_+ActiveTime_+tk-lt_) / MSecsPerDay);

   if LastState then statestr := 'Idle' else statestr := 'Active';
   Caption := Format(FormCaptStr + statestr + ' for %d sec.', [k div 1000]) ;
end;

procedure TForm1.EITOChange(Sender: TObject);
var t: string;
begin
   t := (Sender as TCustomEdit).Text;
   if t = '' then t := '0';
   IdleTimeout := StrToInt(t)*1000;
end;

end.
