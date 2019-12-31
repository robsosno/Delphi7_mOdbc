{********************************************}
{                                            }
{     mODBC - ODBC data aware components     }
{     http://www.perio.unlp.edu.ar/modbc/    }
{                                            }
{********************************************}
unit mDBWiz;
(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)

{$I mODBC.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
{$IFDEF D6UP}
    DesignIntf,
{$ELSE}
    DsgnIntf,
{$ENDIF}

  mDatabas, mQuery;

type
  TdbWizForm = class(TForm)
    Panel7: TPanel;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Panel2: TPanel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Memo1: TMemo;
    Panel3: TPanel;
    DataSourceComboBox: TComboBox;
    MasterLabel: TLabel;
    CreateButton: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    DataSourceNameEdit: TEdit;
    Panel6: TPanel;
    mQueryNameEdit: TEdit;
    Panel8: TPanel;
    SuggestCheckBox: TCheckBox;
    NameComboBox: TComboBox;
    DonotcreateDataSourceCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure NameComboBoxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SuggestCheckBoxClick(Sender: TObject);
    procedure NameComboBoxDropDown(Sender: TObject);
  private
    { Private declarations }
    first: boolean;
  public
    { Public declarations }
    DataBase: TmDataBase;
{$IFDEF D4UP}
{$IFDEF D6UP}
    theFormDesigner: IDesigner;
{$ELSE}
    theFormDesigner: IFormDesigner;
{$ENDIF}
{$ELSE}
    theFormDesigner: TFormDesigner;
{$ENDIF}
    showDataSets: boolean;
  end;

var
  dbWizForm: TdbWizForm;

implementation
uses
  TypInfo, db;
{$R *.DFM}

procedure TdbWizForm.FormCreate(Sender: TObject);
begin
  first := true;
end;

procedure TdbWizForm.FormActivate(Sender: TObject);
begin
  if first then begin
    first := false;
    if showDataSets then begin
      DataSourceComboBox.items.Clear;
      theFormDesigner.GetComponentNames(GetTypeData(TDataSource.ClassInfo),
        DataSourceComboBox.items.Append);
    end else begin

    end;
    MasterLabel.visible := showDataSets;
    DataSourceComboBox.visible := showDataSets;
    NameComboBoxChange(sender);
  end;
end;

procedure TdbWizForm.CreateButtonClick(Sender: TObject);
var
  aMquery: TMquery;
  adataSource: tdataSource;
begin
  if showDataSets then begin
    if DataSourceComboBox.text = '' then begin
      showmessage('You must select a "Master" dataset in order to create a "Detail" mquery');
      exit;
    end;
  end;
  aMquery := TMquery(theFormDesigner.CreateComponent(TMquery, theFormDesigner.GetRoot, 0, 0, 0, 0));
  aMquery.name := mQueryNameEdit.text;
  aMquery.DataBase := DataBase;
  if not DonotcreateDataSourceCheckBox.checked then
  begin
    adataSource := tdataSource(theFormDesigner.CreateComponent(TdataSource, theFormDesigner.GetRoot, 0, 0, 0, 0));
    adataSource.name := DataSourceNameEdit.text;
    adataSource.DataSet := aMquery;
  end;
  if showDataSets then begin
    aMquery.DataSource := TDataSource(theFormDesigner.GetComponent(DataSourceComboBox.text));
  end;
  ModalResult := mrok;
end;

procedure TdbWizForm.NameComboBoxChange(Sender: TObject);
begin
  if SuggestCheckBox.checked then begin
    if NameComboBox.text <> '' then begin
      DataSourceNameEdit.text := NameComboBox.text + 'DataSource';
      mQueryNameEdit.text := NameComboBox.text + 'mQuery';
    end else begin
      DataSourceNameEdit.text := theFormDesigner.UniqueName('DataSource');
      mQueryNameEdit.text := theFormDesigner.UniqueName('mQuery');
    end;
  end;
end;

procedure TdbWizForm.NameComboBoxDropDown(Sender: TObject);
begin
  if NameComboBox.items.count<1 then
  begin
    DataBase.GetTableNames(NameComboBox.items);
  end;
end;


procedure TdbWizForm.SuggestCheckBoxClick(Sender: TObject);
begin
  DataSourceNameEdit.enabled := not SuggestCheckBox.checked;
  mQueryNameEdit.enabled := not SuggestCheckBox.checked;
end;


end.

