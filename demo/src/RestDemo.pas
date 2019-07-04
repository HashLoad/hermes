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
  FMX.Memo, FMX.WebBrowser;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Hermes1: THermes;
    Memo1: TMemo;
    WebBrowser1: TWebBrowser;
    procedure Button1Click(Sender: TObject);
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
    .Execute;

  Memo1.Lines.Add('Status: ' + Hermes1.Response.StatusCode.ToString + ' - ' + Hermes1.Response.StatusText);
  Memo1.Lines.Add('Date: ' + Hermes1.Response.Date);
  Memo1.Lines.Add('Encoding: ' + Hermes1.Response.ContentEncoding);
  Memo1.Lines.Add('Lang: ' + Hermes1.Response.ContentLanguage);
  Memo1.Lines.Add('Length: ' + Hermes1.Response.ContentLength.ToString);
  Memo1.Lines.Add('Mime: ' + Hermes1.Response.MimeType);
  Memo1.Lines.Add('CharSet: ' + Hermes1.Response.ContentCharSet);
  Memo1.Lines.Add('Content: ' + Hermes1.Response.ContentAsString());

  WebBrowser1.LoadFromStrings(Hermes1.Response.ContentAsString, Hermes1.BasePath);
end;

end.
