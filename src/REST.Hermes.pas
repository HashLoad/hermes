unit REST.Hermes;

interface

uses
  System.Classes, REST.Client, REST.Types, FireDAC.Comp.Client, REST.Response.Adapter, Data.Bind.ObjectScope;

type
  THermes = class(TBaseObjectBindSourceDelegate)
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FDataSetAdapter: TRESTResponseDataSetAdapter;

    FDataSet: TFDMemTable;

    procedure DoJoinComponents;
    function GetUrl: String;
    procedure SetUrl(const Value: String);
    function GetMethod: TRESTRequestMethod;
    procedure SetMethod(const Value: TRESTRequestMethod);
    function GetDataSet: TFDMemTable;
    procedure SetDataSet(const Value: TFDMemTable);
    procedure SetAuthProvider(const Value: TCustomAuthenticator);
    function GetAuthProvider: TCustomAuthenticator;
  protected
    function CreateBindSource: TBaseObjectBindSource; override;
  public
    constructor Create(AOwner: TComponent); override;
    Procedure Execute;
    Procedure ExecuteAsync;

  published
    property Url: String read GetUrl write SetUrl;
    property Method: TRESTRequestMethod read GetMethod write SetMethod default rmGET;
    property Dataset: TFDMemTable read GetDataSet write SetDataSet;

    property Client: TRESTClient read FRestClient write FRestClient;
    property Request: TRESTRequest read FRESTRequest write FRESTRequest;
    property Response: TRESTResponse read FRESTResponse write FRESTResponse;
    property DataSetAdapter: TRESTResponseDataSetAdapter read FDataSetAdapter write FDataSetAdapter;
    property AuthProvider: TCustomAuthenticator read GetAuthProvider write SetAuthProvider;
  end;

procedure Register;

implementation

{ THermes }

constructor THermes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRESTClient := TRESTClient.Create(Self);
  FRESTClient.SetSubComponent(True);

  FRESTRequest := TRESTRequest.Create(Self);
  FRESTRequest.SetSubComponent(True);

  FRESTResponse := TRESTResponse.Create(Self);
  FRESTResponse.SetSubComponent(True);

  FDataSetAdapter := TRESTResponseDataSetAdapter.Create(Self);
  FDataSetAdapter.SetSubComponent(True);
  FDataSetAdapter.Response := FRESTResponse;

  DoJoinComponents;
end;

function THermes.CreateBindSource: TBaseObjectBindSource;
begin
  Result := TSubRESTRequestBindSource.Create(Self);
end;

procedure THermes.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

procedure THermes.Execute;
begin
  FRESTRequest.Execute;
end;

procedure THermes.ExecuteAsync;
begin
  FRESTRequest.ExecuteAsync;
end;

function THermes.GetAuthProvider: TCustomAuthenticator;
begin
  Result := FRESTClient.Authenticator;
end;

function THermes.GetDataSet: TFDMemTable;
begin
  Result := FDataSet;
end;

function THermes.GetMethod: TRESTRequestMethod;
begin
  Result := FRESTRequest.Method;
end;

function THermes.GetUrl: String;
begin
  Result := FRESTClient.BaseURL;
end;

procedure THermes.SetAuthProvider(const Value: TCustomAuthenticator);
begin
  FRESTClient.Authenticator := Value;
end;

procedure THermes.SetDataSet(const Value: TFDMemTable);
begin
  FDataSetAdapter.Dataset := Value;
  FDataSet := Value;
end;

procedure THermes.SetMethod(const Value: TRESTRequestMethod);
begin
  FRESTRequest.Method := Value;
end;

procedure THermes.SetUrl(const Value: String);
begin
  FRESTClient.BaseURL := Value;
end;

procedure Register;
begin
  RegisterComponents('HashLoad', [THermes]);
end;

end.
