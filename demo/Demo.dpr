program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  RestDemo in 'src\RestDemo.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
