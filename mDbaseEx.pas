unit mDbaseEx;
{$I mODBC.INC}

(*
2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6


2000-04-15 Per-Eric Larsson
  Added CreateStoredProc to create a stored procedure for
  the mdatabase ( I'm lazy )
*)
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
{$IFDEF D6UP}
    DesignIntf,
    DesignEditors,
{$ELSE}
    DsgnIntf,
{$ENDIF}
  mDatabas;

type
  TmDataBaseEditor = class(TComponentEditor)
  private
    procedure CreateStoredProc;
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
//    procedure Edit;override;
  end;


implementation
uses
  mdbaseed,mDBWiz,mStoredProc;

function TmDataBaseEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

function TmDataBaseEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&DataBase Editor ..';
    1: Result := '&Connect';
    2: Result := '&Disconnect';
    3: Result := '&Add MQuery ..';
    4: Result := '&Add MStoredProc';
    else Result := '';
  end;
end;

procedure TmDataBaseEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: begin
         DataBaseEditorForm := TDataBaseEditorForm.Create( Application);
         try
           DataBaseEditorForm.DataBase := TmDataBase(Component);
           if DataBaseEditorForm.ShowModal=mrOk then
           begin
             Designer.Modified;
           end;
         finally
           DataBaseEditorForm.free;
         end;
       end;
    1: begin
         TmDataBase(Component).Connect;
       end;
    2: begin
         TmDataBase(Component).Disconnect;
       end;
    3: begin
         dbWizForm:=TdbWizForm.Create( Application);
         try
           dbWizForm.DataBase := TmDataBase(Component);
           dbWizForm.showDataSets:=false;
           dbWizForm.theFormDesigner:=designer;
           if dbWizForm.ShowModal=mrOk then
           begin
             Designer.Modified;
           end;
         finally
           dbWizForm.free;
         end;
       end;
    4: begin
        CreateStoredProc;
       end;
  end;
end;



procedure TmDataBaseEditor.CreateStoredProc;
var
  AStoredProc:TmStoredProc;
begin
  AStoredProc:=TmStoredProc(Designer.CreateComponent(TmStoredProc,Designer.GetRoot,0,0,0,0));
  AStoredProc.DataBase:=tmDataBase(component);
  Designer.modified;
end;

end.
