unit REST.Hermes.URL;

interface

uses
  REST.Hermes.Params;

type
  THermesURLDecoded = record
    BasePath: string;
    Resource: string;
    Scheme: string;
    procedure DecodeUrl(ABasePath: string; AResource: string);
  end;

  THermesURL = class
  private
    function GetParams(APath: string): TArray<string>;

    function MakeQueryParams(AParams: THermesParamsExposed): string;
    function MakeScheme(AShema: string; AParams: THermesParamsExposed): string;
    function MakeBasePath(ABasePath: string; AParams: THermesParamsExposed): string;
    function MakeResource(AResource: string; AParams: THermesParamsExposed): string;
  public
    function Parse(ABasePath: string; AResource: string; AParams: THermesParams): string;
  end;

implementation

uses
  System.Generics.Collections, System.Sysutils, REST.Hermes.Core, System.Rtti, System.Net.URLClient,
  System.RegularExpressions, System.RegularExpressionsCore;

{ THermesURL }

function THermesURL.GetParams(APath: string): TArray<string>;
const
  REGEXP_PARAM = ':\w+';
var
  LRegex: TRegEx;
  LMatches: TMatchCollection;
  LIndex: Integer;
begin
  LRegex := TRegEx.Create(REGEXP_PARAM);
  LMatches := LRegex.Matches(APath);

  SetLength(Result, LMatches.Count);

  for LIndex := 0 to LMatches.Count - 1 do
  begin
    Result[LIndex] := LMatches.Item[LIndex].Value;
  end;
end;

function THermesURL.MakeBasePath(ABasePath: string; AParams: THermesParamsExposed): string;
var
  LParams: TArray<string>;
  LParam: string;
begin
  if ABasePath.IsEmpty then
    Exit(EmptyStr);

  LParams := GetParams(ABasePath);

  for LParam in LParams do
  begin
    ABasePath := ABasePath.Replace(LParam, AParams.Params[LParam.Replace(':', '')].ToString);
  end;

  if ABasePath.EndsWith(PATH_SEPARATOR) then
    ABasePath := ABasePath.Remove(High(ABasePath), 1);

  Result := ABasePath;
end;

function THermesURL.MakeQueryParams(AParams: THermesParamsExposed): string;
var
  LQuery: TPair<string, TValue>;
  LParamCount: Integer;
begin
  if AParams.GetQueries.Count = 0 then
    Exit(EmptyStr);

  Result := QUERY_STRING_BEGIN;
  LParamCount := 0;

  for LQuery in AParams.GetQueries do
  begin
    if LParamCount > 0 then
      Result := Result + QUERY_STRING_SEPARATOR;

    Result := Result + LQuery.Key + QUERY_STRING_EQUALS + LQuery.Value.ToString;

    Inc(LParamCount);
  end;
end;

function THermesURL.MakeResource(AResource: string; AParams: THermesParamsExposed): string;
var
  LParams: TArray<string>;
  LParam: string;
begin
  if AResource.IsEmpty then
    Exit(EmptyStr);

  LParams := GetParams(AResource);

  for LParam in LParams do
  begin
    AResource := AResource.Replace(LParam, AParams.Params[LParam.Replace(':', '')].ToString);
  end;
  if not AResource.StartsWith(PATH_SEPARATOR) then
    AResource := PATH_SEPARATOR + AResource;

  Result := AResource;
end;

function THermesURL.MakeScheme(AShema: string; AParams: THermesParamsExposed): string;
var
  LParams: TArray<string>;
  LParam: string;
begin
  if AShema.IsEmpty then
    Exit(EmptyStr);

  LParams := GetParams(AShema);

  for LParam in LParams do
  begin
    AShema := AShema.Replace(LParam, AParams.Params[LParam.Replace(':', '')].ToString);
  end;
  Result := AShema + '://';
end;

function THermesURL.Parse(ABasePath: string; AResource: string; AParams: THermesParams): string;
var
  LExposedParams: THermesParamsExposed;
  LUrlDecoded: THermesURLDecoded;
  LQueryParams: string;
  LBasePath: string;
  LResource: string;
  LShema: string;
begin
  LExposedParams := THermesParamsExposed(AParams);
  LUrlDecoded.DecodeUrl(ABasePath, AResource);

  LShema := MakeScheme(LUrlDecoded.Scheme, LExposedParams);
  LBasePath := MakeBasePath(LUrlDecoded.BasePath, LExposedParams);
  LResource := MakeResource(LUrlDecoded.Resource, LExposedParams);

  LQueryParams := MakeQueryParams(LExposedParams);

  Result := LShema + LBasePath + LResource + LQueryParams;
end;

{ THermesURLDecoded }

procedure THermesURLDecoded.DecodeUrl(ABasePath: string; AResource: string);
const
  EMPTY = 0;
  ONLY_ONE = 1;
var
  LBasePathParts: TArray<string>;
begin
  LBasePathParts := ABasePath.Split(['://']);

  if (Length(LBasePathParts) in [EMPTY, ONLY_ONE]) then
    Self.BasePath := ABasePath
  else
  begin
    Self.Scheme := LBasePathParts[0];
    Self.BasePath := LBasePathParts[1];
  end;

  Self.Resource := AResource;
end;

end.
