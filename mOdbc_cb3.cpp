//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Modbc_cb3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("vcldb35.bpi");
USEPACKAGE("vcldbx35.bpi");
USEUNIT("odbcsql.pas");
USEUNIT("mDataBas.pas");
USEFORMNS("MDBASEED.PAS", Mdbaseed, DataBaseEditorForm);
USEUNIT("mDbaseEx.pas");
USEFORMNS("MDBWIZ.PAS", Mdbwiz, dbWizForm);
USEFORMNS("MEXCEPT.PAS", Mexcept, mODBCErrorDlg);
USEUNIT("mODBCreg.pas");
USEUNIT("mParams.pas");
USEFORMNS("mParmFrm.pas", Mparmfrm, ParamsEditorForm);
USEUNIT("mpeditor.pas");
USEUNIT("mQuery.pas");
USEFORMNS("MQUERYED.PAS", Mqueryed, QueryEditDlg);
USEUNIT("Mqueryex.pas");
USEUNIT("MSESSION.PAS");
USEUNIT("MSQLPARM.PAS");
USEUNIT("mStoredProc.pas");
USEUNIT("mStoredProcEx.pas");
USEUNIT("MTABLE.PAS");
USEUNIT("odbchelper.pas");
USEUNIT("MCONST.PAS");
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
