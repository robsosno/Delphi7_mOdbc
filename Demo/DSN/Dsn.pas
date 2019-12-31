unit Dsn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Registry, mDataBas;

type
  TMainForm = class(TForm)
    Button1: TButton;
    mDataBase1: TmDataBase;
    RemoveDsnBtn: TButton;
    Button4: TButton;
    RemoveSysDsnBtn: TButton;
    DsnList: TListBox;
    Label1: TLabel;
    ConfigDsnBtn: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure RemoveDsnBtnClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure RemoveSysDsnBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConfigDsnBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

  function SQLConfigDataSource(
               hwndParent:       Integer;
               fRequest:         Integer;
               lpszDriverString: String;
               lpszAttributes:   String): Integer;stdcall;external 'ODBCCP32.DLL';

implementation

uses UnitAdd;

{$R *.DFM}

procedure TMainForm.Button1Click(Sender: TObject);
begin
    mDataBase1.GetDriverNames( AddDlg.DriverCB.Items);
    AddDlg.DriverCB.Sorted := true;
    if AddDlg.ShowModal<>mrOk then exit;
    mDataBase1.ConfigureDSN( cdADD_DSN,
                             AddDlg.EditName.Text,
                             AddDlg.DriverCB.Items[AddDlg.DriverCB.ItemIndex],
                             TStringList(AddDlg.ParamMemo.Lines));
   FormCreate(Sender);
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
    mDataBase1.GetDriverNames( AddDlg.DriverCB.Items);
    AddDlg.DriverCB.Sorted := true;
    if AddDlg.ShowModal<>mrOk then exit;
    mDataBase1.ConfigureDSN( cdADD_SYSDSN,
                             AddDlg.EditName.Text,
                             AddDlg.DriverCB.Items[AddDlg.DriverCB.ItemIndex],
                             TStringList(AddDlg.ParamMemo.Lines));
   FormCreate(Sender);
end;


procedure TMainForm.RemoveDsnBtnClick(Sender: TObject);
begin
   mDataBase1.ConfigureDSN( cdREMOVE_DSN,
                             DsnList.Items[DsnList.ItemIndex],
                             '',nil);
   FormCreate(Sender);
end;
procedure TMainForm.RemoveSysDsnBtnClick(Sender: TObject);
begin
   mDataBase1.ConfigureDSN( cdREMOVE_SYSDSN,
                            DsnList.Items[DsnList.ItemIndex],
                            '',nil);
   FormCreate(Sender);
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
    mDataBase1.GetDatSourceNames(DsnList.Items);
end;

procedure TMainForm.ConfigDsnBtnClick(Sender: TObject);
begin
   mDataBase1.ConfigureDSN( cdCONFIG_DSN,
                            DsnList.Items[DsnList.ItemIndex],
                            '',nil);
   FormCreate(Sender);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
   mDataBase1.ConfigureDSN( cdCONFIG_SYSDSN,
                            DsnList.Items[DsnList.ItemIndex],
                            '',nil);
   FormCreate(Sender);
end;

end.
