unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  UserAcReg;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
var i:dword;
begin
   i := GetIdleCount;
   Caption := Format('System IDLE last %d sec. = %d ms.', [i div 1000, i]) ;
   if(GetIdleState) then Self.Color := $FF5566
                    else Self.Color := $6655FF;
end;

end.
