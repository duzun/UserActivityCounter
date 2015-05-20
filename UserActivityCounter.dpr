program UserActivityCounter;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  UserAcReg in 'UserAcReg.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.ShowMainForm := False;
  Application.Title := 'User Activity Counter';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
