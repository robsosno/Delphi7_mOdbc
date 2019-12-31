//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Modbc_cb4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("MCONST.PAS");
USEUNIT("mDatabas.pas");
USEFORMNS("MDBASEED.PAS", Mdbaseed, DataBaseEditorForm);
USEUNIT("mDbaseEx.pas");
USEFORMNS("mDBWiz.pas", Mdbwiz, dbWizForm);
USEFORMNS("mExcept.pas", Mexcept, mODBCErrorDlg);
USEUNIT("MHSTMT.PAS");
USEUNIT("mODBCreg.pas");
USEUNIT("mParams.pas");
USEFORMNS("mParmFrm.pas", Mparmfrm, ParamsEditorForm);
USEUNIT("mpeditor.pas");
USEUNIT("mQuery.pas");
USEFORMNS("Mqueryed.pas", Mqueryed, QueryEditDlg);
USEUNIT("Mqueryex.pas");
USEUNIT("MSESSION.PAS");
USEUNIT("mSQLParm.pas");
USEUNIT("mStoredProc.pas");
USEUNIT("mStoredProcEx.pas");
USEUNIT("Mtable.pas");
USEUNIT("odbchelper.pas");
USEUNIT("ODBCsql.pas");
USEPACKAGE("bcbsmp40.bpi");
USEPACKAGE("Vcldb40.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
