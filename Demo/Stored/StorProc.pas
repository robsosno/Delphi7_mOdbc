unit StorProc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, Db, ComCtrls, ExtCtrls,
  mDataBas, mQuery, mStored;

type
  TForm1 = class(TForm)
    mDataBase1: TmDataBase;
    mStoredProc1: TmStoredProc;
    mQuery1: TmQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    Button1: TButton;
    StockName: TEdit;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
    AParList : array[0..0] of TProcedureParameter;
begin
    FillChar( AParList,sizeof( AParList ),#0 );

    AParList[0].ParType := SQL_DATA_TYPE_STRING;
    StrPCopy( AParList[0].ParValueStr,StockName.Text );
    AParList[0].FieldLength := 255;

    if mStoredProc1.Execute( @AParList[0],1 ) then
    begin
    	mQuery1.Close;
	mQuery1.Open;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	mQuery1.Open;
end;

end.
