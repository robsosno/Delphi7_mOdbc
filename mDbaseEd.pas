unit mDbaseEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  mDatabas, ExtCtrls;

type
  TDataBaseEditorForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    ParamsMemo: TMemo;
    OkBtn: TButton;
    CancelBtn: TButton;
    DefButton: TButton;
    ClearButton: TButton;
    Panel2: TPanel;
    DriversListBox: TListBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    procedure FormActivate(Sender: TObject);
    procedure DefButtonClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ParamsMemoChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DataBase: TmDataBase;
    oldheigth:integer;
    OldParams:String;
  end;
var
  DataBaseEditorForm: TDataBaseEditorForm;

implementation

uses mConst;

{$R *.DFM}

procedure TDataBaseEditorForm.FormActivate(Sender: TObject);
begin
   DataBase.GetDatSourceNames(ComboBox1.Items);
   DataBase.GetDriverNames(ComboBox2.Items);
   ParamsMemo.Lines.Assign( DataBase.Params);
   OldParams := DataBase.Params.Text;
end;

procedure TDataBaseEditorForm.DefButtonClick(Sender: TObject);
Var
   AList: TStrings;
   i: integer;
   s: String;
begin
    if ComboBox1.Text='' then raise Exception.Create(SmNoDataSource);
    with DataBase do
    begin
      DataBaseName := ComboBox1.Text;
      DisConnect;
      Connect;
      Alist := TStringList.Create;
      try
        GetDsnParams(AList);
        s:=AList.Text;
      finally
        Alist.free;
      end;
      ParamsMemo.Lines.Clear;
      repeat
        i := pos( ';', s);
        if i>0 then
        begin
          ParamsMemo.Lines.add(Copy(s,1,i-1));
          s := Copy(s,i+1,Length( s));
        end else
        begin
          ParamsMemo.Lines.add( s);
          s:='';
        end;
      until Length( s)=0;
      i:=0;
// added to remove empty lines PEL
      while i< ParamsMemo.Lines.count do begin
        if ParamsMemo.Lines[i]='' then begin
          ParamsMemo.Lines.delete(i);
        end else begin
          inc(i);
        end;
      end;
//
    end;
end;

procedure TDataBaseEditorForm.OkBtnClick(Sender: TObject);
begin
  DataBase.Params.Assign( ParamsMemo.Lines);
end;

procedure TDataBaseEditorForm.CancelBtnClick(Sender: TObject);
begin
  if ModalResult <>mrOk then DataBase.Params.Text := OldParams;
end;

procedure TDataBaseEditorForm.ClearButtonClick(Sender: TObject);
begin
  ParamsMemo.Clear;
end;

procedure TDataBaseEditorForm.ParamsMemoChange(Sender: TObject);
begin
   with ComboBox1 do
      ItemIndex := Items.IndexOf(ParamsMemo.Lines.Values['DSN']);
   with ComboBox2 do
      ItemIndex := Items.IndexOf(ParamsMemo.Lines.Values['DRIVER']);
end;

procedure TDataBaseEditorForm.ComboBox1Change(Sender: TObject);
begin
   ParamsMemo.Lines.Values['DSN']:=ComboBox1.Text;
end;

procedure TDataBaseEditorForm.ComboBox2Change(Sender: TObject);
var i:integer;
begin
   ParamsMemo.Lines.Values['DRIVER']:=ComboBox2.Text;
// remove DSN line
   i:=paramsmemo.lines.IndexOfName('DSN');
   if i>=0 then
     ParamsMemo.Lines.delete(i);
end;

end.
