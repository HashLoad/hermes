unit REST.Hermes.Editor;

interface

uses
  DesignEditors, DesignIntf;

type
  THermesEditor = class(TComponentEditor)
  public
    procedure DoExecute;
    procedure ClearData;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  REST.Hermes, Vcl.Dialogs;

{ THermesEditor }

procedure THermesEditor.ClearData;
begin
  (Component as THermes).Response.ResetToDefaults;
end;

procedure THermesEditor.DoExecute;
begin
  (Component as THermes).Request.Execute;
  ShowMessage((Component as THermes).Response.StatusText);
end;

procedure THermesEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:
      DoExecute;
    1:
      ClearData;
  else
    ShowMessage('TODO');
  end;
end;

function THermesEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := '&Execute';
    1:
      Result := '&Clear data';
//    2:
//      Result := 'Debugger editor';
  end;
end;

function THermesEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  RegisterComponentEditor(THermes, THermesEditor);
end;

end.
