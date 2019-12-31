unit mODBCreg;
(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)
{$I mODBC.INC}

interface

procedure Register;

implementation
uses Classes,
{$IFDEF D6UP}
    DesignIntf,
{$ELSE}
    DsgnIntf,
{$ENDIF}
     mSession, mDataBas, mQuery, mTable, mpeditor, mParmFrm,
     mStoredProc,

{$IFDEF QUERYEDITOR}
     mQueryEx,
{$ENDIF}
     mParams,  mDbaseEx, mStoredProcEx;

{$R mODBCreg.res}

procedure Register;
begin
  RegisterComponents('mODBC', [TmSession]);
  RegisterComponents('mODBC', [TmDataBase]);
  RegisterComponents('mODBC', [TmQuery]);
  RegisterComponents('mODBC', [TmTable]);
  RegisterComponents('mODBC', [TmStoredProc]);

  RegisterPropertyEditor(TypeInfo(String),TmDataBase,'DataBaseName',TODBCSourceEditor);
  RegisterPropertyEditor(TypeInfo(String),TmTable,'TableName',TmTableNameEditor);
  RegisterPropertyEditor(TypeInfo(TmParams),nil,'',TmParamsEditor);
  RegisterPropertyEditor(TypeInfo(String),TmStoredProc,'StoredProcName',TmProcNameEditor);
{$IFDEF QUERYEDITOR}
  RegisterComponentEditor(TmQuery, TmQueryEditor);
{$ENDIF}
  RegisterComponentEditor(TmDataBase, TmDataBaseEditor);
  RegisterComponentEditor(TmStoredProc, TmStoredProcEditor);
end;

end.
