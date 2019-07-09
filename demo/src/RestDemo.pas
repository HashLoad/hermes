unit RestDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, REST.Types, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Response.Adapter, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Hermes, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  FMX.Bind.Grid, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Grid, Data.Bind.DBScope,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, REST.Authenticator.OAuth, REST.Hermes.Manager, FMX.StdCtrls,
  FMX.Memo, FMX.WebBrowser, REST.Hermes.Interceptor, REST.Hermes.Response;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Hermes1: THermes;
    Memo1: TMemo;
    WebBrowser1: TWebBrowser;
    HermesInterceptor: THermesInterceptor;
    procedure Button1Click(Sender: TObject);
    procedure Hermes1RequestCompleted(const AHermes: THermes);
    procedure HermesInterceptorBeforeExecute(const AHermes: THermes);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Hermes1
    .SetQuery('q', 'Rodrigo Bernardi')
    .ExecuteAsync;

end;

procedure TForm3.Hermes1RequestCompleted(const AHermes: THermes);
begin
  Memo1.Lines.Add(Hermes1.Response.ToString);

  WebBrowser1.LoadFromStrings(Hermes1.Response.Body, Hermes1.BasePath);
end;

procedure TForm3.HermesInterceptorBeforeExecute(const AHermes: THermes);
begin
  AHermes.SetParam('base', 'www.google.com');
end;

end.
