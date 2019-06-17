unit REST.Hermes.Register;

interface

procedure Register;

implementation

uses
  REST.Hermes, System.Classes, REST.Hermes.Manager, REST.Hermes.Interceptor;

procedure Register;
begin
  RegisterComponents('HashLoad', [THermes, THermesInterceptor, THermesManager]);
end;


end.
