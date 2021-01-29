unit REST.Hermes.Core;

interface

uses
  System.Net.HttpClientComponent, System.Net.HttpClient, System.SysUtils, REST.Hermes.URL, System.Classes;

type
  TRequestMethod = (rmGET, rmPOST, rmDELETE, rmHEAD, rmPUT, rmMERGE, rmPATCH);

const
  PATH_SEPARATOR = '/';
  PARAM_STRING_BEGIN = PATH_SEPARATOR + ':';
  QUERY_STRING_BEGIN = '?';
  QUERY_STRING_SEPARATOR = '&';
  QUERY_STRING_EQUALS = '=';

  TRequestMethodString: array [TRequestMethod] of string = ('GET', 'POST', 'DELETE', 'HEAD', 'PUT', 'MERGE', 'PATCH');

type
  THermesAsyncThread = class(TThread)
  private
    FProcess: TProc;
    FAfterExecute: TProc;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;

  public
    function OnExecute(AProcess: TProc): THermesAsyncThread;
    function OnAfterExecute(AAfterExecute: TProc): THermesAsyncThread;

    constructor Create; reintroduce;
  end;

implementation

{ THermesCore }

{ THermesAsyncThread }

constructor THermesAsyncThread.Create;
begin
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure THermesAsyncThread.DoTerminate;
begin
  inherited;
  FAfterExecute;
end;

procedure THermesAsyncThread.Execute;
begin
  inherited;
  FProcess;
end;

function THermesAsyncThread.OnExecute(AProcess: TProc): THermesAsyncThread;
begin
  FProcess := AProcess;
  Result := Self;
end;

function THermesAsyncThread.OnAfterExecute(AAfterExecute: TProc): THermesAsyncThread;
begin
  FAfterExecute := AAfterExecute;
  Result := Self;
end;

end.
