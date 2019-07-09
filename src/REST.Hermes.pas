unit REST.Hermes;

interface

uses
  System.Classes, REST.Client, REST.Types, REST.Response.Adapter, Data.Bind.ObjectScope,
  System.SysUtils, System.Generics.Collections, System.Json, System.Net.HttpClientComponent,
  System.Net.URLClient, FireDAC.Comp.Client, System.Net.HttpClient, REST.Hermes.Params, REST.Hermes.Core, System.Rtti,
  REST.Hermes.Response;

type
  THermes = class;

  TRequestMethod = REST.Hermes.Core.TRequestMethod;

  THermesExecuteCallback = procedure(const AHermes: THermes) of Object;
  THermesExecuteCallbackRef = TProc<THermes>;

  IHermesInterceptor = interface
    procedure BeforeExecute(const AHermes: THermes);
    procedure AfterExecute(const AHermes: THermes);
  end;

  THermes = class(TComponent)
  private
    FBasePath: string;
    FResource: string;

    FClient: TNetHTTPClient;
    FResponse: THermesResponse;

    FOwnsObject: Boolean;
    FBody: TJSONObject;

    FMethod: TRequestMethod;
    FParams: THermesParams;


    FOnRequestCompleted: THermesExecuteCallback;

    procedure AfterExecute(const AHermes: THermes);
    procedure BeforeExecute(const AHermes: THermes);

    function GetURL: string;
    procedure DoInjectHeaders;

    procedure DoExecute;
    procedure DoExecuteAsync; overload;
    procedure DoExecuteAsync(ACallback: TProc); overload;

    procedure OnInternalRequestError(const Sender: TObject; const AError: string);
    procedure OnInternalRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
  public
    constructor Create(AOwner: TComponent); override;

    function SetQuery(AParam: string; AValue: TValue): THermes;
    function SetParam(AParam: string; AValue: TValue): THermes;
    function SetHeader(AKey: string; AValue: TValue): THermes;
    function SetBody(AJson: TJSONObject; const AOwnsObject: Boolean = True): THermes;

    procedure Execute; overload;
    procedure ExecuteAsync; Overload;
    procedure ExecuteAsync(ACallback: THermesExecuteCallbackRef); Overload;
    procedure ExecuteAsync(ACallback: TProc); Overload;
    destructor Destroy; override;
  published
    property Method: TRequestMethod read FMethod write FMethod default TRequestMethod.rmGET;
    property BasePath: String read FBasePath write FBasePath;
    property Resource: string read FResource write FResource;
    property Params: THermesParams read FParams;

    property Response: THermesResponse read FResponse;

    property OnRequestCompleted: THermesExecuteCallback read FOnRequestCompleted write FOnRequestCompleted;

    class Procedure AddGlobalInterceptor(AInterceptor: IHermesInterceptor);
    class Procedure RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
  end;

implementation

uses
  REST.Hermes.Manager, REST.Hermes.URL, System.Threading;

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

  if Assigned(OnRequestCompleted) then
    OnRequestCompleted(Self);
end;

procedure THermes.BeforeExecute(const AHermes: THermes);
var
  LInterceptor: IHermesInterceptor;
begin
  for LInterceptor in THermesManager.FGlobalInterceptors do
  begin
    LInterceptor.BeforeExecute(Self);
  end;
end;

constructor THermes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClient := TNetHTTPClient.Create(nil);
  FClient.SetSubComponent(True);
  FOwnsObject := True;

  FParams := THermesParams.Create;

  FClient.OnRequestError := OnInternalRequestError;
  FClient.OnRequestCompleted := OnInternalRequestCompleted;

  FBasePath := THermesManager.FBasePath;

  FResponse := THermesResponse.Create;
end;

destructor THermes.Destroy;
begin
  FClient.DisposeOf;
  FParams.DisposeOf;
  FResponse.DisposeOf;
  inherited;
end;

procedure THermes.DoExecute;
var
  LMethod: string;
  LURL: string;
begin
  BeforeExecute(Self);
  LMethod := TRequestMethodString[FMethod];

  LURL := GetURL;
  DoInjectHeaders;

  if Assigned(FBody) then
  begin
    FClient.Execute(LMethod, LURL, TStringStream.Create(FBody.ToJSON));
    if FOwnsObject then
      FBody.DisposeOf;
  end
  else
    FClient.Execute(LMethod, LURL);
end;

procedure THermes.DoExecuteAsync(ACallback: TProc);
var
  LMethod: string;
  LURL: string;
begin
  BeforeExecute(Self);
  THermesAsyncThread.Create
    .OnExecute(
      procedure
      begin
        LMethod := TRequestMethodString[FMethod];
        LURL := GetURL;
        DoInjectHeaders;

        if Assigned(FBody) then
        begin
          FClient.Execute(LMethod, LURL, TStringStream.Create(FBody.ToJSON));
          if FOwnsObject then
            FBody.DisposeOf;
        end
        else
          FClient.Execute(LMethod, LURL);
      end
    ).OnAfterExecute(
      procedure
      begin
        if Assigned(ACallBack) then
        begin
          TThread.Synchronize(nil, TThreadProcedure(ACallBack));
        end;
      end)
    .Start;
end;

procedure THermes.DoExecuteAsync;
begin
  DoExecuteAsync(nil);
end;

procedure THermes.Execute;
begin
  DoExecute;
end;

procedure THermes.ExecuteAsync(ACallback: TProc);
begin
  DoExecuteAsync(ACallback);
end;

function THermes.GetURL: string;
var
  LURLParser: THermesURL;
begin
  LURLParser := THermesURL.Create;
  try
    Result := LURLParser.Parse(BasePath + PATH_SEPARATOR + Resource, Params);
  finally
    LURLParser.DisposeOf;
  end;
end;

procedure THermes.DoInjectHeaders;
var
  LHeader: TPair<string, TValue>;
begin
  for LHeader in THermesParamsExposed(Params).GetHeaders do
  begin
    FClient.CustomHeaders[LHeader.Key] := LHeader.Value.ToString;
  end;
end;

procedure THermes.ExecuteAsync(ACallback: THermesExecuteCallbackRef);
begin
  DoExecuteAsync(
    procedure
    begin
      ACallback(Self);
    end);
end;

procedure THermes.ExecuteAsync;
begin
  DoExecuteAsync;
end;

procedure THermes.OnInternalRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
begin
  THermesResponseHack(FResponse).SetReponse(AResponse);
  AfterExecute(Self);
end;

procedure THermes.OnInternalRequestError(const Sender: TObject; const AError: string);
begin
  THermesResponseHack(FResponse).SetError(AError);
  AfterExecute(Self);
end;

class procedure THermes.RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
begin
  THermesManager.FGlobalInterceptors.Remove(AInterceptor);
end;

function THermes.SetHeader(AKey: string; AValue: TValue): THermes;
begin
  Result := Self;
  FParams.SetHeader(AKey, AValue);
end;

function THermes.SetParam(AParam: string; AValue: TValue): THermes;
begin
  Result := Self;
  FParams.SetParam(AParam, AValue);
end;

function THermes.SetQuery(AParam: string; AValue: TValue): THermes;
begin
  FParams.SetQuery(AParam, AValue);
  Result := Self;
end;

function THermes.SetBody(AJson: TJSONObject; const AOwnsObject: Boolean = True): THermes;
begin
  Result := Self;
  FBody := AJson;
end;

end.
