unit MQueryEd;

{$I mODBC.INC}

(*
This unit was developed to make it easier to set up parameters
for a SQL query -

Revision History:

2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6

Ver 1.2 -1999-05-20
The component used a "mquery" internally to run queries,
this made it hard to store and recall when user pressed "cancel"
This version uses four memos to handle the differet queries!
This means that the dialog is better for editing already typed queries

Ver 1.1 - 1999-02-11
Konstantin made some changes (nice ones) prior to its public release

Ver 1.0 - 1999-02-11
This really was a version just to prove it could be done
*)

interface

{$IFDEF QUERYEDITOR}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,db,
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
  mquery;

type
  TQueryEditDlg = class(TForm)
    Panel1: TPanel;
    Panel5: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    PageControl1: TPageControl;
    OptionsTabSheet: TTabSheet;
    GroupBox1: TGroupBox;
    Panel9: TPanel;
    Label2: TLabel;
    TableNameComboBox: TComboBox;
    GetTableFieldsButton: TButton;
    DatasetDefaultsButton: TButton;
    GetPrimaryKeysButton: TButton;
    SQLTabSheet: TTabSheet;
    Panel2: TPanel;
    SelectPanel: TPanel;
    Panel17: TPanel;
    KeyPanel: TPanel;
    Panel13: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel3: TPanel;
    FieldsListBox: TListBox;
    KeyFieldsListBox: TListBox;
    UpdateFieldsListBox: TListBox;
    AllFieldsCheckBox: TCheckBox;
    QuoteFieldNamesCheckBox: TCheckBox;
    GenerateSQLButton: TButton;
    ResetDatabaseButton: TButton;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    SelectMemo: TMemo;
    ModifyMemo: TMemo;
    InsertMemo: TMemo;
    DeleteMemo: TMemo;
    Panel4: TPanel;
    Label1: TLabel;
    Panel6: TPanel;
    Panel7: TPanel;
    Label3: TLabel;
    Panel8: TPanel;
    Panel12: TPanel;
    Label4: TLabel;
    Panel14: TPanel;
    Panel15: TPanel;
    Label5: TLabel;
    Panel16: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure TableNameComboBoxChange(Sender: TObject);
    procedure TableNameComboBoxDropDown(Sender: TObject);
    procedure GenerateSQLButtonClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure TableNameComboBox1DropDown(Sender: TObject);
    procedure GetTableFieldsButtonClick(Sender: TObject);
    procedure GetPrimaryKeysButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure DatasetDefaultsButtonClick(Sender: TObject);
    procedure ResetDatabaseButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SqlMemoChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    SQLGenerated:boolean;
    function fnutt(const s:string):String;
  public
    FMquery:Tmquery;
    { Public declarations }
  end;

var
  QueryEditDlg: TQueryEditDlg;

{$IFDEF COMPTEST}
procedure ShowQueryDlg(AQuery: TmQuery);
{$ENDIF}
implementation
{$R *.DFM}


{$IFDEF COMPTEST}
procedure ShowQueryDlg(AQuery: TmQuery);
begin
  if AQuery.DataBase = nil then
  begin
    ShowMessage('Assign Database first.');
    exit;
  end;
  if (AQuery <> nil) then
  begin
    QueryEditDlg := TQueryEditDlg.Create(Application);
    try
      QueryEditDlg.caption := 'Editing mQuery "' + AQuery.name + '"';
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
//        Designer.Modified;
      end;
    finally
      QueryEditDlg.FMquery.Destroy;
      QueryEditDlg.Free;
    end;
  end;
end;
{$ENDIF}

function TQueryEditDlg.fnutt(const s: string): String;
begin
  if QuoteFieldNamesCheckBox.checked then
  begin
    result := '"' + s + '"';
  end else
  begin
    result:=s;
  end;
end;

procedure TQueryEditDlg.TableNameComboBoxChange(Sender: TObject);
begin
  FMquery.DataBase.GetColumnNames(TableNameComboBox.text,FieldsListBox.Items);
end;

procedure TQueryEditDlg.TableNameComboBoxDropDown(Sender: TObject);
begin
  FMquery.DataBase.GetTableNames(TableNameComboBox.Items);
end;

procedure TQueryEditDlg.GenerateSQLButtonClick(Sender: TObject);
var
  i: integer;
  ts: tstringlist;
begin
(*
General behaviour is :
add all fieldnames to ts,
insert commas,
send all items in ts to the "real" Tstring
*)
   ts := tstringlist.Create;
   if (FieldsListBox.selcount=0) and  (AllFieldsCheckBox.Checked=false) then
   begin
     ts.add('No Fields selected for "select" query');
   end;

   if (KeyFieldsListBox.selcount=0) then
   begin
     ts.add('No Primary key fields selected');
   end;

   if (UpdateFieldsListBox.selcount=0) then
   begin
     ts.add('No Update fields selected');
   end;

   if ts.count>0 then
   begin
     ts.add('Continue Anyway ?');
     If windows.MessageBox(0,PChar(ts.Text),'Warning',mb_okcancel)=idCancel then
     begin
       exit;
     end;
   end;

   SQLGenerated:=true;
//   FMquery.sql.BeginUpdate;
//   FMquery.sql.Clear;
//   FMquery.sql.Add('SELECT ');

   ts.Clear;
   ts.Add('SELECT ');

   if AllFieldsCheckBox.checked then
   begin
     ts.add('  * ');
   end else
   begin
     for i:=0 to FieldsListBox.Items.Count-1 do
     begin
       if FieldsListBox.Selected[i] then
       begin
         ts.add('  '+fnutt(FieldsListBox.Items[i]));
       end;
     end;

     for i := 1 to ts.Count-2 do
     begin
       ts[i]:= ts[i]+',';
     end;
   end;

   ts.add( 'FROM ' + fnutt( TableNameComboBox.text));

   Selectmemo.Lines.Assign(ts);
   FMquery.SQL.Assign(ts);

//MODIFY query generation

   ModifyMemo.Lines.BeginUpdate;
   ModifyMemo.Lines.Clear;

   ts.clear;
   for i := 0 to UpdateFieldsListBox.items.Count-1 do
   begin
     if UpdateFieldsListBox.Selected[i] then //Added this to make it work, JB
       ts.add(fnutt(UpdateFieldsListBox.items[i])+' = :'+UpdateFieldsListBox.items[i]);
   end;

   for i := 0 to ts.count-2 do
   begin
     ts[i] := ts[i]+',';
   end;
// no fields - no query PEL
   if ts.Count>0 then
   begin
     ModifyMemo.Lines.add('update '+fnutt(TableNameComboBox.text));
     ModifyMemo.Lines.add('set ');

     ModifyMemo.Lines.AddStrings(ts);
     ModifyMemo.Lines.add('Where ');
     ts.clear;
     for i := 0 to KeyFieldsListBox.items.Count-1 do
     begin
       if KeyFieldsListBox.Selected[i] then
       begin
         ts.add(fnutt(KeyFieldsListBox.items[i])+' = :OLD_'+KeyFieldsListBox.items[i]);
       end;
     end;

     for i := 0 to ts.count-2 do
     begin
       ts[i] := ts[i] + ' and';
     end;
     ModifyMemo.Lines.AddStrings(ts);
   end;
   ModifyMemo.Lines.EndUpdate;

//INSERT query generation
   ts.Clear;
   for i := 0 to UpdateFieldsListBox.items.Count-1 do
   begin
     if UpdateFieldsListBox.Selected[i] then  //Added this to make it work, JB
       ts.add(fnutt(UpdateFieldsListBox.items[i]));
   end;

   for i := 0 to ts.count-2 do
   begin
     ts[i] := ts[i] + ',';
   end;
// no fields - no query PEL
   InsertMemo.Lines.BeginUpdate;
   InsertMemo.Lines.Clear;
   if ts.Count>0 then
   begin
     InsertMemo.Lines.add('insert into ' + fnutt(TableNameComboBox.text));
     InsertMemo.Lines.add('(');
     InsertMemo.Lines.AddStrings(ts);
     InsertMemo.Lines.add(')');
     InsertMemo.Lines.add('values ');
     InsertMemo.Lines.add('(');
     for i:=0 to ts.count-1 do
     begin
       ts[i]:=':'+ts[i];
     end;
     InsertMemo.Lines.AddStrings(ts);
     InsertMemo.Lines.add(')');
   end;
   InsertMemo.Lines.EndUpdate;

// delete query
   ts.Clear;
   for i := 0 to KeyFieldsListBox.items.count-1 do
     if KeyFieldsListBox.Selected[i] then
       ts.add( fnutt( KeyFieldsListBox.items[i]) + ' = :OLD_' + KeyFieldsListBox.items[i]);

   for i := 0 to ts.count-2 do
   begin
     ts[i] := ts[i] + ' and';
   end;
// no fields - no query PEL
   DeleteMemo.Lines.BeginUpdate;
   DeleteMemo.Lines.Clear;
   if ts.Count > 0 then
   begin
     DeleteMemo.Lines.add('delete from ' + fnutt(TableNameComboBox.text));
     DeleteMemo.Lines.add('where');

     DeleteMemo.Lines.AddStrings( ts);
   end;
   DeleteMemo.Lines.EndUpdate;
   ts.free;
   PageControl1.ActivePage := SQLTabSheet;
end;

procedure TQueryEditDlg.ResetDatabaseButtonClick(Sender: TObject);
begin
  SQLGenerated := false;
  FMquery.DataBase.DisConnect;
  TableNameComboBox.Text := '';
  FieldsListBox.Items.Clear;
  KeyFieldsListBox.Items.Clear;
  UpdateFieldsListBox.Items.Clear;
end;


procedure TQueryEditDlg.Panel2Resize(Sender: TObject);
begin
  SelectPanel.width := Panel2.width div 3;
  KeyPanel.width := SelectPanel.width;
end;

procedure TQueryEditDlg.TableNameComboBox1DropDown(Sender: TObject);
begin
  FMquery.DataBase.GetTableNames(TableNameComboBox.Items);
end;

procedure TQueryEditDlg.GetTableFieldsButtonClick(Sender: TObject);
begin
  if TableNameComboBox.text = '' then
  begin
    raise exception.create('Must Specify a Table name');
  end else
  begin
    FMquery.DataBase.GetColumnNames(TableNameComboBox.text,UpdateFieldsListBox.Items);
    KeyFieldsListBox.items:=UpdateFieldsListBox.Items;
    FieldsListBox.items:=UpdateFieldsListBox.Items;
  end;
end;

procedure TQueryEditDlg.GetPrimaryKeysButtonClick(Sender: TObject);
var
  ts: tstringlist;
  i: integer;
begin
  ts := tstringlist.Create;
  FMquery.DataBase.GetPrimaryKeys(TableNameComboBox.text,ts);
  for i := 0 to KeyFieldsListBox.items.count-1 do
  begin
    KeyFieldsListBox.Selected[i] := ts.IndexOf(KeyFieldsListBox.items[i]) <> -1
  end;
  ts.free;
end;


procedure TQueryEditDlg.PageControl1Change(Sender: TObject);
begin
  if assigned(PageControl1.ActivePage.OnEnter) then
  begin
    PageControl1.ActivePage.OnEnter(Sender);
  end;
end;

procedure TQueryEditDlg.FormActivate(Sender: TObject);
begin
  if assigned(PageControl1.OnChange)then
  begin
    PageControl1.OnChange(Sender);
  end;
end;


procedure TQueryEditDlg.DatasetDefaultsButtonClick(Sender: TObject);
var
  i: integer;
begin
  GetTableFieldsButtonClick(Sender);
  GetPrimaryKeysButtonClick(Sender);
  AllFieldsCheckBox.checked := true;
  for i := 0 to UpdateFieldsListBox.Items.Count-1 do
  begin
    UpdateFieldsListBox.Selected[i] := true;
  end;
end;

procedure TQueryEditDlg.OKButtonClick(Sender: TObject);
begin
  if not SQLGenerated then
  begin
    If windows.MessageBox( 0, 'You have not generated any Sql! '#13'Continue Anyway ?',
                           'Warning', mb_okcancel) = idCancel then
    begin
      modalresult := mrnone;
    end;
  end;
end;

procedure TQueryEditDlg.FormCreate(Sender: TObject);
begin
  SQLGenerated := false;
end;

procedure TQueryEditDlg.SqlMemoChange(Sender: TObject);
begin
  SQLGenerated := true;
end;
{$ELSE}
implementation
{$ENDIF}
end.
