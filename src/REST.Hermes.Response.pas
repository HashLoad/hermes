unit REST.Hermes.Response;

interface

uses
  System.Net.HttpClient, System.Classes, System.Net.URLClient, System.Generics.Collections;

type

  THermesResponse = class
  private
    FContentType: string;
    FStatusCode: Integer;
    FStatusText: string;
    
    FContentCharSet: string; 
    FBody: TMemoryStream;

    FHeaders: TDictionary<string, string>;
    
    function GetContentLength: Integer;
    function GetHeaders(AKey: String): string;
  public
    constructor Create;
    function Body<TBody: class>(Out ABody: TBody): Boolean; overload;
    function Body: string; overload;

    property ContentType: string read FContentType;
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property ContentLength: Integer read GetContentLength;  
    
    property Headers[AKey: String]: string read GetHeaders;
    
    function ToString: string; override;
    
    destructor Destroy; override;
  end;

  THermesResponseHack = class(THermesResponse) 
  private
    procedure LoadHeaders(AHeaders: TNetHeaders);
  public
    procedure SetReponse(AResponse: IHTTPResponse);
    procedure SetError(AError: string);
  end;


implementation

uses
  System.JSON, REST.Hermes.Parser, System.SysUtils;

{ THermesResponse }

function THermesResponse.Body: string;
begin
  Result := THermesParser.ParseStreamToString(FBody, FContentCharSet);
end;

function THermesResponse.Body<TBody>(out ABody: TBody): Boolean;
begin
  Result := THermesParser.Parse<TBody>(Body, ABody);
end;

constructor THermesResponse.Create;
begin
  FBody := TMemoryStream.Create;
  FHeaders := TDictionary<string, string>.Create;
end;

destructor THermesResponse.Destroy;
begin
  FBody.DisposeOf;
  FHeaders.DisposeOf;
  inherited;
end;

function THermesResponse.GetContentLength: Integer;
begin
  Result := FBody.Size;
end;

function THermesResponse.GetHeaders(AKey: String): string;
begin
  Result := EmptyStr;
  FHeaders.TryGetValue(AKey, Result);
end;

function THermesResponse.ToString: string;
const
  TO_STRING = 'ContentType: %s' + sLineBreak +
    'StatusCode: %d' + sLineBreak +
    'StatusText: %s' + sLineBreak +
    'ContentLength: %d' + sLineBreak +
    'Body: %s';
begin
  Result := Format(TO_STRING, [ContentType, StatusCode, StatusText, ContentLength, Body]); 
end;

{ THermesResponseHack }

procedure THermesResponseHack.LoadHeaders(AHeaders: TNetHeaders);
var
  LHeader: TNameValuePair;
begin
  FHeaders.Clear;

  for LHeader in AHeaders do
  begin
    FHeaders.Add(LHeader.Name, LHeader.Value);
  end;
end;

procedure THermesResponseHack.SetError(AError: string);
const 
 STATUS_ERROR = 0;
 STATUS_ERROR_TEXT = 'Erro on request';
 MIME_TYPE = 'plain/text';
 CHAR_SET = 'win1252';
 
 
begin
  FStatusCode := STATUS_ERROR;
  FBody.Clear;
  FBody.Write(STATUS_ERROR_TEXT, STATUS_ERROR_TEXT.Length);
  FContentType := MIME_TYPE;
  FStatusText := STATUS_ERROR_TEXT; 
  FContentCharSet := CHAR_SET;
  FHeaders.Clear;
end;

procedure THermesResponseHack.SetReponse(AResponse: IHTTPResponse);
begin
  FBody.Clear;
  FBody.CopyFrom(AResponse.ContentStream, AResponse.ContentStream.Size);
  FStatusCode := AResponse.StatusCode;
  FContentType := AResponse.MimeType;
  FStatusCode := AResponse.StatusCode;
  FStatusText := AResponse.StatusText;
  FContentCharSet := AResponse.ContentCharset;
  LoadHeaders(AResponse.Headers);
end;

end.
