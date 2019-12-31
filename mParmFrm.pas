unit mParmFrm;

(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6


2000-08-06 Per-Eric Larsson
  Added popup menu so that parameternames can be copied into
  clipboard easily!

2000-04-15 Per-Eric Larsson
  added display of parameter type !
*)
{$I mODBC.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db,
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
  mParams, Menus,odbchelper;

const
  NameTypes: array[TFieldType] of String = (
    'Unknown', 'String', 'Smallint', 'Integer', 'Word',
    'Boolean', 'Float', 'Currency', 'BCD', 'Date', 'Time', 'DateTime',
    'Bytes', 'VarBytes', 'Integer', 'Blob', 'Memo', 'Graphic',
    'FmtMemo', '', '', '', ''
(*Small Change PEL 00-08-16*)
{$IFDEF D4UP}
    , '', '', '', '', '', '', ''
{$ENDIF}
{$IFDEF D5UP}
    , '' ,'', '', '', '', ''
{$ENDIF}
{$IFDEF D6UP}
    , '', ''
{$ENDIF}
    );

const
  ParamTypes:array[TParamType] of String = (
  'Unknown','Input', 'Output', 'InputOutput', 'Result');

type
  TmParamsEditor = class(TClassProperty)
  public
      function GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
  end;

  TParamsEditorForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ListBox: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    TypeParam: TComboBox;
    ValueParam: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    IsNullCB: TCheckBox;
    Label4: TLabel;
    ParamTypeStaticText: TStaticText;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    CopyAll1: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure TypeParamChange(Sender: TObject);
    procedure ValueParamChange(Sender: TObject);
    procedure IsNullCBClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure CopyAll1Click(Sender: TObject);
  private
    { Private declarations }
  public
    Params: TmParams;
    { Public declarations }
  end;

var
  ParamsEditorForm: TParamsEditorForm;

implementation
Uses
  ClipBrd;
{$R *.DFM}

function TmParamsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TmParamsEditor.Edit;
var
  Form: TParamsEditorForm;
begin
  Form := TParamsEditorForm.Create( Application);
  try
    Form.Params := TmParams.Create;
    Form.Params.Assign( TmParams( GetOrdValue));
    if Form.ShowModal = mrOk then
    begin
      TmParams( GetOrdValue).AssignValues(Form.Params);
      Modified;
    end;
  finally
    Form.Params.Free;
    Form.Free;
  end;
end;

procedure TParamsEditorForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  // Set items of ListBox
  ListBox.Items.Clear;

  for i := 0 to Params.Count-1 do
  begin
    with Params do
      ListBox.Items.addObject(Items[i].name, Items[i]);

    if Params.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBoxClick(Sender);
    end else
    begin
      ListBox.ItemIndex := -1;
      TypeParam.Enabled := False;
      ValueParam.Enabled := False;
      IsNullCB.Enabled := False;
    end;
  end;
end;

procedure TParamsEditorForm.ListBoxClick(Sender: TObject);
begin
  with TmParam(ListBox.Items.Objects[ListBox.ItemIndex]) do
  begin
    TypeParam.ItemIndex := TypeParam.Items.IndexOf(NameTypes[DataType]);
    IsNullCB.Checked := IsNull;
    ValueParam.Text := AsString;
// added 2000-04-15 PEL
    ParamTypeStaticText.caption:=ParamTypes[ParamType];
  end;
end;

procedure TParamsEditorForm.TypeParamChange(Sender: TObject);
var
  d: TFieldType;
begin
  with TmParam(ListBox.Items.Objects[ListBox.ItemIndex]) do
    for d := Low(TFieldType) to High(TFieldType) do
      if NameTypes[d] = TypeParam.Text then
        DataType:=d;
end;

procedure TParamsEditorForm.ValueParamChange(Sender: TObject);
begin
  try
    with TmParam(ListBox.Items.Objects[ListBox.ItemIndex]) do
      Text:=ValueParam.Text;
  except
    ValueParam.SetFocus;
    raise;
  end;
end;

procedure TParamsEditorForm.IsNullCBClick(Sender: TObject);
begin
  with TmParam(ListBox.Items.Objects[ListBox.ItemIndex]) do
    if IsNullCB.Checked then
      Clear;
end;

procedure TParamsEditorForm.Copy1Click(Sender: TObject);
begin
  try
    ClipBoard.astext:=ListBox.items[ListBox.itemindex];
  except
  end;
end;

procedure TParamsEditorForm.CopyAll1Click(Sender: TObject);
begin
  ClipBoard.astext:=ListBox.items.text;
end;

end.
