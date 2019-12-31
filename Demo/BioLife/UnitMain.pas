unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, DBTables, StdCtrls, DBCtrls, ExtCtrls, mQuery,
  mTable, mDataBas;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBLabel1: TDBText;
    DBImage1: TDBImage;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    mDataBase1: TmDataBase;
    mTable1: TmTable;
    Panel2: TPanel;
    DBMemo1: TDBMemo;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    DBNavigator1: TDBNavigator;
    mDataBase2: TmDataBase;
    Button1: TButton;
    mQuery1: TmQuery;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
   DataSource1.DataSet.Open;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
DataSource1.DataSet.Close;
DataSource1.DataSet.Open;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
   dbimage1.CopyToClipboard;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
   dbimage1.PasteFromClipboard;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   DataSource1.DataSet.insert;
end;

end.
