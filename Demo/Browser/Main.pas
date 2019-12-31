unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, StdCtrls, ExtCtrls, Buttons, Db, mQuery, mDataBas,
  ComCtrls, DBCtrls, mTable;

type
  TForm1 = class(TForm)
    mDB: TmDataBase;
    mQuery: TmQuery;
    Panel2: TPanel;
    Panel3: TPanel;
    ListBox1: TListBox;
    Panel1: TPanel;
    Notebook1: TNotebook;
    DBGrid1: TDBGrid;
    BtnClose: TBitBtn;
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    DBNavigator1: TDBNavigator;
    DBMemo1: TDBMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Panel4: TPanel;
    SBdsn: TSpeedButton;
    SBtables: TSpeedButton;
    SBsql: TSpeedButton;
    mTable1: TmTable;
    Panel5: TPanel;
    BtnOpen: TBitBtn;
    BtnExec: TBitBtn;
    BtnPrev: TBitBtn;
    BtnNext: TBitBtn;
    Edit1: TEdit;
    BtnFind: TBitBtn;
    Button1: TButton;
    procedure BtnOpenClick(Sender: TObject);
    procedure mQueryAfterOpen(DataSet: TDataSet);
    procedure mQueryAfterClose(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure DBGrid1ColEnter(Sender: TObject);
    procedure BtnExecClick(Sender: TObject);
    procedure SBdsnClick(Sender: TObject);
    procedure SBtablesClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure SBsqlClick(Sender: TObject);
    procedure BtnFindClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BtnOpenClick(Sender: TObject);
begin
     mQuery.SQL.Assign(Memo1.Lines);
     mQuery.InsertSQL.Assign(Memo2.Lines);
     mQuery.ModifySQL.Assign(Memo3.Lines);
     mQuery.DeleteSQL.Assign(Memo4.Lines);
     DBMemo1.DataField:='';
     mQuery.Open;
     DataSource1.DataSet := mQuery;
     Notebook1.PageIndex:=1;
end;

procedure TForm1.mQueryAfterOpen(DataSet: TDataSet);
begin
//     Notebook1.PageIndex:=1;
     BtnOpen.Enabled:=False;
     BtnExec.Enabled:=False;
     BtnClose.Enabled:=True;
end;

procedure TForm1.mQueryAfterClose(DataSet: TDataSet);
begin
//     Notebook1.PageIndex:=0;
     BtnClose.Enabled:=False;
     BtnOpen.Enabled:=True;
     BtnExec.Enabled:=True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     mDB.GetDatSourceNames(ListBox1.Items);
     Notebook1.PageIndex:=1;
end;

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
   DataSource1.DataSet.Close;
   if SBsql.Down then Notebook1.PageIndex:=0;
end;

procedure TForm1.DBGrid1ColEnter(Sender: TObject);
begin
     DBMemo1.DataField:=DBGrid1.SelectedField.FieldName;
end;

procedure TForm1.BtnExecClick(Sender: TObject);
begin
   mQuery.SQL.Assign(Memo1.Lines);
   mQuery.InsertSQL.Clear;
   mQuery.ModifySQL.Clear;
   mQuery.DeleteSQL.Clear;
   mQuery.ExecSQL;
end;

procedure TForm1.SBdsnClick(Sender: TObject);
begin
   with ListBox1 do
   begin
      mDB.GetDatSourceNames(Items);
      ItemIndex := Items.IndexOf(mDB.DataBaseName);
   end;
end;

procedure TForm1.SBtablesClick(Sender: TObject);
begin
   mDB.GetTableNames(ListBox1.Items);
   Notebook1.PageIndex:=1;
//   Panel5.Visible := False;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
   if SBdsn.Down then
   begin
      if mDB.Connected then mDB.DisConnect;
      mDB.DataBaseName:=ListBox1.Items[ListBox1.ItemIndex];
      SBtables.Enabled := True;
      SBsql.Enabled := True;
   end;
   if SBtables.Down then
   begin
      mTable1.TableName := ListBox1.Items[ListBox1.ItemIndex];
      DataSource1.DataSet := mTable1;
      DBMemo1.datafield:=''; mTable1.Open;
      Notebook1.PageIndex:=1;
   end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
   if SBdsn.Down then
   begin
      SBtables.Down := True;
      SBtables.Click;
   end;
   if SBsql.Down then
   begin
      Memo1.Lines.Text := 'select * from '+ListBox1.Items[ListBox1.ItemIndex];
   end;
end;

procedure TForm1.SBsqlClick(Sender: TObject);
begin
   mDB.GetTableNames(ListBox1.Items);
   Notebook1.PageIndex:=0;
   DataSource1.DataSet.Close;
end;

procedure TForm1.BtnFindClick(Sender: TObject);
begin
   TmCustomQuery(DataSource1.DataSet).Locate(DBGrid1.SelectedField.FieldName,Edit1.Text,[]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var i:integer;
begin
{
 with DataSource1.dataset do
 for i := 0 to Fields.Count - 1 do
    ShowMessage(Fields[i].FieldName);}
    ShowMessage(DBGrid1.SelectedField.AsString);
end;

end.
