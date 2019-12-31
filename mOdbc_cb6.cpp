//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("MDBASEED.PAS", Mdbaseed, DataBaseEditorForm);
USEFORMNS("mDBWiz.pas", Mdbwiz, dbWizForm);
USEFORMNS("mExcept.pas", Mexcept, mODBCErrorDlg);
USEFORMNS("mParmFrm.pas", Mparmfrm, ParamsEditorForm);
USEFORMNS("Mqueryed.pas", Mqueryed, QueryEditDlg);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
