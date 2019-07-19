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
    Hermes: THermes;
    Memo1: TMemo;
    WebBrowser1: TWebBrowser;
    HermesInterceptor: THermesInterceptor;
    procedure Button1Click(Sender: TObject);
    procedure HermesRequestCompleted(const AHermes: THermes);
    procedure HermesInterceptorBeforeExecute(const AHermes: THermes);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  System.JSON;

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Hermes
    .SetQuery('q', 'Rodrigo Bernardi')
//    .SetBody(TJSONObject.ParseJSONValue('{"a": "b"}') as TJSONObject)
    .ExecuteAsync(
      procedure
      begin
        Memo1.Lines.Add('Anon');
      end);
end;

procedure TForm3.HermesRequestCompleted(const AHermes: THermes);
begin
  Sleep(1000);
  Memo1.Lines.Add(Hermes.Response.ToString);

  WebBrowser1.LoadFromStrings(Hermes.Response.Body, Hermes.BasePath);
end;

procedure TForm3.HermesInterceptorBeforeExecute(const AHermes: THermes);
begin
  AHermes.SetParam('base', 'www.google.com');
end;

end.
