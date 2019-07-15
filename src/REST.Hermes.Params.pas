unit REST.Hermes.Params;

interface

uses
  System.Generics.Collections, System.Rtti, System.Classes;

type
  THermesParams = class(TPersistent)
  private
    FHeaders: TDictionary<string, TValue>;
    FParams: TDictionary<string, TValue>;
    FQuery: TDictionary<string, TValue>;

    function GetHeaders(AKey: String): TValue;
    function GetParams(AKey: String): TValue;
    function GetQueries(AKey: String): TValue;
    procedure SetHeaders(AKey: String; const Value: TValue);
    procedure SetParams(AKey: String; const Value: TValue);
    procedure SetQueries(AKey: String; const Value: TValue);
  public
    constructor Create;

    function SetHeader(AKey: string; AValue: TValue): THermesParams;
    function SetParam(AKey: string; AValue: TValue): THermesParams;
    function SetQuery(AKey: string; AValue: TValue): THermesParams;
    destructor Destroy; override;

    property Params[AKey: String]: TValue read GetParams write SetParams;
    property Queries[AKey: String]: TValue read GetQueries write SetQueries;
    property Headers[AKey: String]: TValue read GetHeaders write SetHeaders;
  end;

  THermesParamsExposed = class(THermesParams)
    function GetHeaders: TDictionary<string, TValue>;
    function GetParams: TDictionary<string, TValue>;
    function GetQueries: TDictionary<string, TValue>;
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

function THermesParams.SetHeader(AKey: string; AValue: TValue): THermesParams;
begin
  FHeaders.AddOrSetValue(AKey, AValue);

  Result := Self;
end;

procedure THermesParams.SetHeaders(AKey: String; const Value: TValue);
begin
  FHeaders.AddOrSetValue(AKey, Value);
end;

function THermesParams.SetParam(AKey: string; AValue: TValue): THermesParams;
begin
  FParams.AddOrSetValue(AKey, AValue);

  Result := Self;
end;

procedure THermesParams.SetParams(AKey: String; const Value: TValue);
begin
  FParams.AddOrSetValue(AKey, Value);
end;

function THermesParams.SetQuery(AKey: string; AValue: TValue): THermesParams;
begin
  FQuery.AddOrSetValue(AKey, AValue);

  Result := Self;
end;

procedure THermesParams.SetQueries(AKey: String; const Value: TValue);
begin
  FQuery.AddOrSetValue(AKey, Value);
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
