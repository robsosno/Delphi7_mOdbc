unit UnitDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, mQuery, mTable, mDataBas;

type
  TDM = class(TDataModule)
    mDataBase1: TmDataBase;
    mQuery1: TmQuery;
    mQuery2: TmQuery;
    DataSource1: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.DFM}

end.
