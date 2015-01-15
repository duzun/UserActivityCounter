program UserActivityCounter;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  UserAcReg in 'UserAcReg.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  with Form1 do begin
     Left := (Screen.Width -Width ) shr 1;
     Top  := (Screen.Height-Height) shr 2;
  end;
  Application.Run;
end.
