unit mpeditor;
(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)
{$I mODBC.INC}

interface
  uses
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
    Classes;

  type
    TODBCSourceEditor = class(TStringProperty)
    public
      procedure GetValues( Proc: TGetStrProc); override;
      function GetAttributes: TPropertyAttributes; override;
    end;

  type
    TmTableNameEditor = class(TStringProperty)
    public
      procedure GetValues( Proc: TGetStrProc); override;
      function GetAttributes: TPropertyAttributes; override;
    end;

  type
    TmProcNameEditor = class(TStringProperty)
    public
      procedure GetValues( Proc: TGetStrProc); override;
      function GetAttributes: TPropertyAttributes; override;
    end;

implementation
uses SysUtils, mDataBas, mTable,mStoredProc;

procedure TODBCSourceEditor.GetValues( Proc: TGetStrProc);
var
  dbase: TmDataBase;
  tlist: TStringList;
  i:     Integer;
begin
  if GetComponent(0) is TmDataBase then
  begin
    dbase := TmDataBase( GetComponent(0));
    tlist := TStringList.Create;
    try
      dbase.GetDatSourceNames(tlist);
      for i := 0 to tlist.Count-1 do
        Proc( tlist[i]);
    finally
      tlist.free;
    end;
  end;
end;

function TODBCSourceEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paValueList, paSortList];
end;

procedure TmTableNameEditor.GetValues( Proc: TGetStrProc);
var
  db:    TmTable;
  tlist: TStringList;
  i:     Integer;
begin
  if GetComponent(0) is TmTable then
  begin
    db := TmTable( GetComponent(0));
    if not Assigned( db.DataBase) then
      raise Exception.Create('DataBase property not assigned');

    tlist := TStringList.Create;
    try
      db.DataBase.GetTableNames(tlist);
      for i := 0 to tlist.Count-1 do
        Proc( tlist[i]);
    finally
      tlist.free;
    end;
  end;
end;

function TmTableNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paValueList, paSortList];
end;


procedure TmProcNameEditor.GetValues( Proc: TGetStrProc);
var
  db:    TmStoredProc;
  tlist: TStringList;
  i:     Integer;
begin
  if GetComponent(0) is TmStoredProc then
  begin
    db := TmStoredProc( GetComponent(0));
    if not Assigned( db.DataBase) then
      raise Exception.Create('DataBase property not assigned');

    tlist := TStringList.Create;
    try
      db.DataBase.GetProcNames(tlist);
      for i := 0 to tlist.Count-1 do
        Proc( tlist[i]);
    finally
      tlist.free;
    end;
  end;
end;

function TmProcNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paValueList, paSortList];
end;

end.
