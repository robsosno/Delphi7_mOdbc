program Customer;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitDM in 'UnitDM.pas' {DM: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
