unit REST.Hermes.Manager;

interface

uses
  System.Classes, REST.Hermes, System.Generics.Collections, System.SysUtils;

type
  THermesManager = class(TComponent)
  public
    class var FGlobalInterceptors: TList<IHermesInterceptor>;
    class var GlobalHermes: TList<THermes>;

    class var FBasePath: string;
  private
    procedure SetBasePath(const Value: string);
    function GetBasePath: string;
  published
    property BasePath: string read GetBasePath write SetBasePath;

    class Procedure AddGlobalInterceptor(AInterceptor: IHermesInterceptor);
    class Procedure RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
  end;

  THermesBasePathInjector = class(TInterfacedObject, IHermesInterceptor)
    procedure BeforeExecute(const AHermes: THermes);
    procedure AfterExecute(const AHermes: THermes);
  end;

implementation

{ THermesManager }

class procedure THermesManager.AddGlobalInterceptor(AInterceptor: IHermesInterceptor);
begin
  FGlobalInterceptors.Add(AInterceptor);
end;

function THermesManager.GetBasePath: string;
begin
  Result := FBasePath;
end;

class procedure THermesManager.RemoveGlobalInterceptor(AInterceptor: IHermesInterceptor);
begin
  FGlobalInterceptors.Remove(AInterceptor);
end;

procedure THermesManager.SetBasePath(const Value: string);
begin
  Self.FBasePath := Value;
end;

{ THermesBasePathInjector }

procedure THermesBasePathInjector.AfterExecute(const AHermes: THermes);
begin
  //
end;

procedure THermesBasePathInjector.BeforeExecute(const AHermes: THermes);
begin
  if AHermes.Client.BaseURL.IsEmpty and not THermesManager.FBasePath.IsEmpty then
    AHermes.Client.BaseURL := THermesManager.FBasePath;
end;

initialization

THermesManager.FGlobalInterceptors := TList<IHermesInterceptor>.Create;
THermesManager.GlobalHermes := TList<THermes>.Create;

THermesManager.AddGlobalInterceptor(THermesBasePathInjector.Create);

finalization

THermesManager
  .FGlobalInterceptors
  .DisposeOf;

THermesManager
  .GlobalHermes
  .DisposeOf;

end.
