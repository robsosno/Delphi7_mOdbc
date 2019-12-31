unit mStoredProcEx;
{$I mODBC.INC}
(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)

interface
uses
  Windows, SysUtils, Classes,
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
  db;
type
  TmStoredProcEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure CopyParamsToClipboard;
  end;

implementation
uses
  clipbrd,mStoredProc,odbchelper,mparams;
{ TmStoredProcEditor }

procedure TmStoredProcEditor.CopyParamsToClipboard;
var
  aStoredProc:TmStoredProc;
  i:integer;
  ts:TstringList;
  Aparam:Tstparam;
begin
  ts:=TstringList.Create;
  aStoredProc:=TmStoredProc(component);
  for i := 0 to aStoredProc.params.count-1 do
  begin
    Aparam:=aStoredProc.params[i];
    ts.Add(
    format('%s.ParamByName(''%s'')%s',
    [component.name,
    Aparam.name,
    FieldTypetoFunctionName(Aparam.datatype)]));
  end;
  clipboard.AsText:=ts.text;
  ts.free;
end;


procedure TmStoredProcEditor.ExecuteVerb(Index: Integer);
begin
  case index of
  0:begin
    CopyParamsToClipboard;
    end;
  end;
end;

function TmStoredProcEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Copy Params To Clipboard';
    else Result := '';
  end;
end;

function TmStoredProcEditor.GetVerbCount: Integer;
begin
  result:=1;

end;

end.
