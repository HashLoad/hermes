unit REST.Hermes.Core;

interface

uses
  System.Net.HttpClientComponent, System.Net.HttpClient, System.SysUtils, REST.Hermes.URL;

type
  TRequestMethod = (rmGET, rmPOST, rmDELETE, rmHEAD, rmPUT, rmMERGE, rmPATCH);

const
  PATH_SEPARATOR = '/';
  QUERY_STRING_BEGIN = '?';
  QUERY_STRING_SEPARATOR = '&';
  QUERY_STRING_EQUALS = '=';

  TRequestMethodString: array [TRequestMethod] of string = ('GET', 'POST', 'DELETE', 'HEAD', 'PUT', 'MERGE', 'PATCH');


implementation

{ THermesCore }



end.
