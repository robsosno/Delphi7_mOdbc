unit UnitAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TAddDlg = class(TForm)
    Label1: TLabel;
    EditName: TEdit;
    Label2: TLabel;
    DriverCB: TComboBox;
    AddBtn: TBitBtn;
    BitBtn2: TBitBtn;
    ParamMemo: TMemo;
    Label3: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure PropChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddDlg: TAddDlg;

implementation

{$R *.DFM}

procedure TAddDlg.FormActivate(Sender: TObject);
begin
    EditName.Text:='';
    AddBtn.Enabled:=False;
end;

procedure TAddDlg.PropChange(Sender: TObject);
begin
    AddBtn.Enabled:= (EditName.Text<>'')
                     and(DriverCB.ItemIndex>=0);
end;

end.
