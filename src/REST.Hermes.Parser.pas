unit REST.Hermes.Parser;

interface

uses
  System.Classes, System.SysUtils;

type
  THermesParser = class
    class function Parse<ParseType: class>(AContent: string; out AParsedValue: ParseType): Boolean;
    class function ParseStreamToString(AStream: TStream; const ACharSet: string): string; overload;
    class function ParseStreamToString(AStream: TStream; const ACharSet: string; const AEncoding: TEncoding): string;
      overload;
  end;

implementation

uses
  System.JSON, System.Net.HttpClient;

{ THermesParser }

class function THermesParser.Parse<ParseType>(AContent: string; out AParsedValue: ParseType): Boolean;
begin
  Result := False;

  if (TypeInfo(ParseType) = TypeInfo(TJSONObject)) or (TypeInfo(ParseType) = TypeInfo(TJSONArray)) then
  begin
    Result := True;
    AParsedValue := TJSONObject.ParseJSONValue(AContent) as ParseType;
  end;
end;

class function THermesParser.ParseStreamToString(AStream: TStream; const ACharSet: string;
  const AEncoding: TEncoding): string;
var
  LReader: TStringStream;
  LStream: TStream;
  LFreeLStream: Boolean;
begin
  Result := '';
  if AEncoding = nil then
  begin
    if (ACharSet <> '') and (string.CompareText(ACharSet, 'utf-8') <> 0) then // do not translate
      LReader := TStringStream.Create('', TEncoding.GetEncoding(ACharSet), True)
    else
      LReader := TStringStream.Create('', TEncoding.UTF8, False);
  end
  else
    LReader := TStringStream.Create('', AEncoding, False);
  try
    LStream := AStream;
    LFreeLStream := False;

    try
      LReader.CopyFrom(LStream, 0);
      Result := LReader.DataString;
    finally
      if LFreeLStream then
        LStream.Free;
    end;
  finally
    LReader.Free;
  end;
end;

class function THermesParser.ParseStreamToString(AStream: TStream; const ACharSet: string): string;
begin
  Result := ParseStreamToString(AStream, ACharSet, nil);
end;

end.
