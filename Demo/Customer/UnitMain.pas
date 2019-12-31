unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, StdCtrls, DBCtrls, ExtCtrls, Buttons, Db;

type
  TForm1 = class(TForm)
    CtrlsPanel: TPanel;
    Bevel1: TBevel;
    Navigator: TDBNavigator;
    BtnPanel: TPanel;
    CustPanel: TPanel;
    CustGrid: TDBGrid;
    OrdersPanel: TPanel;
    OrdersGrid: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses UnitDM;

{$R *.DFM}

procedure TForm1.FormActivate(Sender: TObject);
begin
   DM.mQuery1.Open;
   DM.mQuery2.Open;
end;

end.
