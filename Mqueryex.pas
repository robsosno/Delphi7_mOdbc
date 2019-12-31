unit Mqueryex;
{$I mODBC.INC}
(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  db,
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
  mquery;

{$IFDEF QUERYEDITOR}

type
  TmQueryEditor = class(TComponentEditor)
  private
{$IFDEF D6UP}
    stdEditor: IComponentEditor;
{$ELSE}
    stdEditor: TComponentEditor;
{$ENDIF}
    stdEditorVerbs: integer;
  public
{$ifdef D4UP}
{$IFDEF D6UP}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
{$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
{$ENDIF}
{$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: TFormDesigner); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ShowEditor;
    procedure ShowWizard;
    procedure CopyParamstoClipBoard;
    procedure CopyFieldstoClipBoard;
  end;

implementation

uses
  mqueryed,
  mparams,
  odbchelper,
  clipbrd,
  mDBWiz;

type
  PClass = ^TClass;

{$ifdef D4UP}

{$IFDEF D6UP}
constructor TmQueryEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
var
  saveClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  saveClass := PClass(AComponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    stdEditor := GetComponentEditor(AComponent, ADesigner);
    stdEditorVerbs := stdEditor.GetVerbCount;
  finally
    PClass(AComponent)^ := saveClass;
  end;
end;

{$ELSE}
constructor TmQueryEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
var
  saveClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  saveClass := PClass(AComponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    stdEditor := GetComponentEditor(AComponent, ADesigner);
    stdEditorVerbs := stdEditor.GetVerbCount;
  finally
    PClass(AComponent)^ := saveClass;
  end;
end;
{$ENDIF}

{$ELSE}
constructor TmQueryEditor.Create(AComponent: TComponent; ADesigner: TFormDesigner);
var
  saveClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  saveClass := PClass(AComponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    stdEditor := GetComponentEditor(AComponent, ADesigner);
    stdEditorVerbs := stdEditor.GetVerbCount;
  finally
    PClass(AComponent)^ := saveClass;
  end;
end;
{$ENDIF}



destructor TmQueryEditor.Destroy;
begin
{$IFDEF D6UP}
{$ELSE}
  stdEditor.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TmQueryEditor.ExecuteVerb(Index: Integer);
begin
  if Index < stdEditor.GetVerbCount then
  begin
    stdEditor.ExecuteVerb(Index)
  end
  else
  begin
    case index - stdEditorVerbs of
      0:
        begin
          Showeditor;
        end;
      1:
        begin
          ShowWizard;
        end;
      2:
        begin
          CopyParamstoClipBoard;
        end;
      3:
        begin
        CopyFieldstoClipBoard;
        end;

    end;
  end;
end;

function TmQueryEditor.GetVerb(Index: Integer): string;
begin
  if Index < stdEditorVerbs then
  begin
    Result := stdEditor.GetVerb(Index)
  end
  else
  begin
    case index - stdEditorVerbs of
      0: Result := '&mQuery Editor ...';
      1: Result := '&Add Detail mQuery ...';
      2: Result := '&Copy Params to ClipBoard';
      3: Result := '&Copy Fields to ClipBoard';
    else
      Result := '';
    end;
  end;
end;

function TmQueryEditor.GetVerbCount: Integer;
begin
  Result := stdEditorVerbs + 4;
end;

procedure TmQueryEditor.ShowWizard;
begin
  dbWizForm := TdbWizForm.Create(Application);
  try
    dbWizForm.showDataSets := true;
    dbWizForm.DataBase := TmQuery(Component).Database;
    dbWizForm.theFormDesigner := designer;
    if dbWizForm.ShowModal = mrOk then
    begin
      Designer.Modified;
    end;
  finally
    dbWizForm.free;
  end;
end;

procedure TmQueryEditor.ShowEditor;
var
  AQuery: TmQuery;
begin
  AQuery := TmQuery(Component);
  if AQuery.DataBase = nil then
  begin
    ShowMessage('Assign Database first.');
    exit;
  end;
  if (AQuery <> nil) then
  begin
    QueryEditDlg := TQueryEditDlg.Create(Application);
    try
      QueryEditDlg.caption := 'Editing mQuery "' + Component.name + '"';
      QueryEditDlg.FMquery := Tmquery.Create(QueryEditDlg);
      QueryEditDlg.FMquery.DataBase := AQuery.DataBase;
      QueryEditDlg.FMquery.DataSource := AQuery.DataSource;
      QueryEditDlg.DeleteMemo.Lines := AQuery.DeleteSQL;
      QueryEditDlg.InsertMemo.Lines := AQuery.InsertSQL;
      QueryEditDlg.ModifyMemo.Lines := AQuery.ModifySQL;
      QueryEditDlg.SelectMemo.Lines := AQuery.SQL;
      if QueryEditDlg.ShowModal = mrOK then
      begin
        AQuery.DataBase.DataBaseName := QueryEditDlg.FMquery.DataBase.DataBaseName;
        AQuery.SQL := QueryEditDlg.SelectMemo.Lines;
        AQuery.DeleteSQL := QueryEditDlg.DeleteMemo.Lines;
        AQuery.InsertSQL := QueryEditDlg.InsertMemo.Lines;
        AQuery.ModifySQL := QueryEditDlg.ModifyMemo.Lines;
        Designer.Modified;
      end;
    finally
      QueryEditDlg.FMquery.Destroy;
      QueryEditDlg.Free;
    end;
  end;
end;

procedure TmQueryEditor.CopyParamstoClipBoard;
var
  AQuery: TmQuery;
  i: integer;
  ts: TstringList;
  Aparam: TStParam;
begin
  ts := TstringList.Create;
  AQuery := TmQuery(component);
  for i := 0 to AQuery.params.count - 1 do
  begin
    Aparam := AQuery.params[i];
    ts.Add(
      format('%s.%s.ParamByName(''%s'')%s',
      [component.owner.name,
      component.name,
      Aparam.name,
        FieldTypetoFunctionName(Aparam.datatype)]));
  end;
  clipboard.AsText := ts.text;
  ts.free;
end;

procedure TmQueryEditor.CopyFieldstoClipBoard;
var
  AQuery: TmQuery;
  i: integer;
  ts: TstringList;
  AField: TField;
begin
  ts := TstringList.Create;
  AQuery := TmQuery(component);
  for i := 0 to AQuery.fields.count - 1 do
  begin
    AField := AQuery.Fields[i];
    ts.Add(
      format('%s.%s%s',
      [component.owner.name,
      AField.name,
        FieldTypetoFunctionName(AField.datatype)]));
  end;
  clipboard.AsText := ts.text;
  ts.free;
end;


{$ELSE}
implementation
{$ENDIF}
end.

