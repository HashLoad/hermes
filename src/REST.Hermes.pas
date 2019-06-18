unit REST.Hermes;

interface

uses
  System.Classes, REST.Client, REST.Types, FireDAC.Comp.Client, REST.Response.Adapter, Data.Bind.ObjectScope,
  System.SysUtils, System.Generics.Collections;

type
  THermes = class;

  THermesExecuteCallback = procedure(const AHermes: THermes) of Object;
  THermesExecuteCallbackRef = TProc<THermes>;

  IHermesInterceptor = interface
    procedure BeforeExecute(const AHermes: THermes);
    procedure AfterExecute(const AHermes: THermes);
  end;

  THermes = class(TComponent)
  private                                        
    FDataSet: TFDMemTable;
    FBasePath: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FDataSetAdapter: TRESTResponseDataSetAdapter;
    FOnRequestCompleted: THermesExecuteCallback;

    procedure DoJoinComponents;
    function GetMethod: TRESTRequestMethod;
    function GetDataSet: TFDMemTable;
    function GetBasePath: String;
    function GetResource: string;
    function GetAuthProvider: TCustomAuthenticator;
    function BasePathIsStored: Boolean;

    procedure SetMethod(const Value: TRESTRequestMethod);
    procedure SetDataSet(const Value: TFDMemTable);
    procedure SetResource(const Value: string);
    procedure SetBasePath(const Value: String);
    procedure SetAuthProvider(const Value: TCustomAuthenticator);

    
    procedure RESTRequestAfterExecute(Sender: TCustomRESTRequest);

     procedure AfterExecute(const AHermes: THermes);
     procedure BeforeExecute(const AHermes: THermes);
  protected
   public
    constructor Create(AOwner: TComponent); override;

    Procedure Execute;
    Procedure ExecuteAsync; Overload;
    Procedure ExecuteAsync(ACallback: THermesExecuteCallbackRef); Overload;
    destructor Destroy; override;
  published
    property Method: TRESTRequestMethod read GetMethod write SetMethod default rmGET;
    property Dataset: TFDMemTable read GetDataSet write SetDataSet;
    property BasePath: String read GetBasePath write SetBasePath stored BasePathIsStored;
    property Resource: string read GetResource write SetResource;
    
    property Client: TRESTClient read FRESTClient write FRESTClient;
    property Request: TRESTRequest read FRESTRequest write FRESTRequest;
    property Response: TRESTResponse read FRESTResponse write FRESTResponse;
    property DataSetAdapter: TRESTResponseDataSetAdapter read FDataSetAdapter write FDataSetAdapter;
    property AuthProvider: TCustomAuthenticator read GetAuthProvider write SetAuthProvider;
    property OnRequestCompleted: THermesExecuteCallback read FOnRequestCompleted write FOnRequestCompleted;

    class Procedure AddGlobalInterceptor(AInterceptor: IHermesInterceptor);
    class Procedure RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
  end;

implementation

uses
  REST.Hermes.Manager;

{ THermes }

class procedure THermes.AddGlobalInterceptor(AInterceptor: IHermesInterceptor);
begin
  THermesManager.FGlobalInterceptors.Add(AInterceptor);
end;

procedure THermes.AfterExecute(const AHermes: THermes);
var
  LInterceptor: IHermesInterceptor;
begin
  for LInterceptor in THermesManager.FGlobalInterceptors do
  begin
    LInterceptor.AfterExecute(Self);
  end;
end;

function THermes.BasePathIsStored: Boolean;
begin
 Result := (FBasePath <> THermesManager.FBasePath) or FBasePath.IsEmpty;
end;

procedure THermes.BeforeExecute(const AHermes: THermes);
var
  LInterceptor: IHermesInterceptor;
begin
  FRESTClient.BaseURL := BasePath;

  for LInterceptor in THermesManager.FGlobalInterceptors do
  begin
    LInterceptor.BeforeExecute(Self);
  end;
end;

constructor THermes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  THermesManager
    .GlobalHermes
    .Add(Self);

  FRESTClient := TRESTClient.Create(Self);
  FRESTClient.SetSubComponent(True);

  FRESTRequest := TRESTRequest.Create(Self);
  FRESTRequest.SetSubComponent(True);

  FRESTResponse := TRESTResponse.Create(Self);
  FRESTResponse.SetSubComponent(True);

  FDataSetAdapter := TRESTResponseDataSetAdapter.Create(Self);
  FDataSetAdapter.SetSubComponent(True);
  FDataSetAdapter.Response := FRESTResponse;

  FRESTRequest.OnAfterExecute := RESTRequestAfterExecute;
  FBasePath := THermesManager.FBasePath;

  DoJoinComponents;
end;

destructor THermes.Destroy;
begin
  THermesManager
    .GlobalHermes
    .Remove(Self);
  inherited;
end;

procedure THermes.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

procedure THermes.Execute;
begin
  BeforeExecute(Self);
    
  FRESTRequest.Execute;
  if Assigned(OnRequestCompleted) then
    OnRequestCompleted(Self);
end;

procedure THermes.ExecuteAsync(ACallback: THermesExecuteCallbackRef);
begin
  BeforeExecute(Self);
  FRESTRequest.ExecuteAsync(
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(Self)
      else
        OnRequestCompleted(Self);
    end);
end;

procedure THermes.ExecuteAsync;
begin
  BeforeExecute(Self);
  FRESTRequest.ExecuteAsync(
    procedure
    begin
      OnRequestCompleted(Self);
    end);
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

function THermes.GetResource: string;
begin
  Result := FRESTRequest.Resource;
end;

class procedure THermes.RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
begin
   THermesManager.FGlobalInterceptors.Remove(AInterceptor);
end;

procedure THermes.RESTRequestAfterExecute(Sender: TCustomRESTRequest);
begin
  AfterExecute(Self);
end;

function THermes.GetBasePath: String;
begin
  if FBasePath.IsEmpty then
    FBasePath := THermesManager.FBasePath;

  Result := FBasePath;
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

procedure THermes.SetResource(const Value: string);
begin
  FRESTRequest.Resource := Value;
end;

procedure THermes.SetBasePath(const Value: String);
begin
  FBasePath := Value;
end;

end.
