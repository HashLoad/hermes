unit REST.Hermes.Interceptor;

interface

uses
  REST.Hermes, System.Classes;

type
  THermesInterceptor = class(TComponent, IHermesInterceptor)
  private
    FOnBeforeExecute: THermesExecuteCallback;
    FOnAfterExecute: THermesExecuteCallback;
    FActive: Boolean;

    procedure BeforeExecute(const AHermes: THermes);
    procedure AfterExecute(const AHermes: THermes);
    procedure SetActive(const Value: Boolean);
  public
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property OnBeforeExecute: THermesExecuteCallback read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: THermesExecuteCallback read FOnAfterExecute write FOnAfterExecute;
  end;

implementation

uses
  REST.Hermes.Manager;

{ THermesInterceptor }

procedure THermesInterceptor.AfterExecute(const AHermes: THermes);
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(AHermes);
end;

procedure THermesInterceptor.BeforeExecute(const AHermes: THermes);
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(AHermes);
end;

destructor THermesInterceptor.Destroy;
begin
  THermes.RemoveGlobalInterceptor(Self);
  inherited;
end;

procedure THermesInterceptor.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if FActive then
    THermesManager.AddGlobalInterceptor(Self)
  else
    THermesManager.RemoveGlobalInterceptor(Self);
end;

end.
