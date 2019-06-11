program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  RestDemo in 'RestDemo.pas' {Form3},
  REST.Hermes in '..\src\REST.Hermes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
