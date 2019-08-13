unit REST.Hermes.Params;

interface

uses
  System.Generics.Collections, System.Rtti, System.Classes;

type
 THermesList = TDictionary<string, TValue>;

  THermesParams = class(TPersistent)
  private
    FHeaders: THermesList;
    FQuery: THermesList;
    FParams: THermesList;

    function GetHeaders(AKey: String): TValue;
    function GetParams(AKey: String): TValue;
    function GetQueries(AKey: String): TValue;
  public
    constructor Create;
    destructor Destroy; override;

    property Params: THermesList read FParams write FParams;
    property Query: THermesList read FQuery write FQuery;
    property Headers: THermesList read FHeaders write FHeaders;
  end;

  THermesParamsExposed = class(THermesParams)
    function GetHeaders: THermesList;
    function GetParams: THermesList;
    function GetQueries: THermesList;
  end;

implementation

uses
  System.SysUtils, REST.Hermes.Core;

{ THermesParams }

constructor THermesParams.Create;
begin
  FHeaders := TDictionary<string, TValue>.Create;
  FParams := TDictionary<string, TValue>.Create;
  FQuery := TDictionary<string, TValue>.Create;
end;

destructor THermesParams.Destroy;
begin
  FHeaders.DisposeOf;
  FParams.DisposeOf;
  FQuery.DisposeOf;
  inherited;
end;

function THermesParams.GetHeaders(AKey: String): TValue;
begin
  Result := FHeaders.Items[AKey];
end;

function THermesParams.GetParams(AKey: String): TValue;
begin
  Result := FParams.Items[AKey];
end;

function THermesParams.GetQueries(AKey: String): TValue;
begin
  Result := FQuery.Items[AKey];
end;

{ THermesParamsExposed }

function THermesParamsExposed.GetHeaders: TDictionary<string, TValue>;
begin
  Result := FHeaders;
end;

function THermesParamsExposed.GetParams: TDictionary<string, TValue>;
begin
  Result := FParams;
end;

function THermesParamsExposed.GetQueries: TDictionary<string, TValue>;
begin
  Result := FQuery;
end;

end.
