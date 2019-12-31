program pDsn;

uses
  Forms,
  Dsn in 'Dsn.pas' {MainForm},
  UnitAdd in 'UnitAdd.pas' {AddDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAddDlg, AddDlg);
  Application.Run;
end.
