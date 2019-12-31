{#Z+ hide from scanhelp !}
{$RANGECHECKS OFF}
{$DEFINE ODBCVER3}

(*
//The following defines are declared in this file:
//
//KNOWNCOMPILER is defined if the compiler was known when this
//version of mODBC was released.
//if not You will get an error, you might try to compile anyway
//- just follow the instructions at the end of the file,
//- read the warning aswell !
//
//QUERYEDITOR is set if the mQuery editor can be used
//
//USETPARAM is used to tell wheather to use the standard
//query TParams class or the mParams class
//Borland moved the declaration from a BDE dependent unit to the
//db unit in Delphi 4
//
// Per-Eric Larsson
*)


(*
 this define makes odbc loading dynamic
 *)
{$DEFINE DYNLOADINGODBC}

{$RANGECHECKS OFF}

(*  Set defines dependent on Delphi/Cbuilder Version
*****************************************************)

{$IFDEF VER100}
// 				DELPHI 3.0
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE QUERYEDITOR}
{$ENDIF}

{$IFDEF VER110}
// 				CBUILDER 3.0
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER3}
  {$DEFINE QUERYEDITOR}
{$ENDIF}

{$IFDEF VER120}
// 				DELPHI 4.0
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE D4UP}
  {$DEFINE QUERYEDITOR}
  {$DEFINE USETPARAM}
{$ENDIF}


{$IFDEF VER125}
// 				CBUILDER 4.0
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER4}
  {$DEFINE D4UP}
  {$DEFINE QUERYEDITOR}
  {$DEFINE USETPARAM}
{$ENDIF}

{$IFDEF VER130}
// 				DELPHI 5.0 or CBUILDER 5.0
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE QUERYEDITOR}
  {$DEFINE USETPARAM}
  {$IFNDEF BCB}
// 				DELPHI 5.0
    {$DEFINE DELPHI}
    {$DEFINE DELPHI5}
  {$ELSE}
// 				CBUILDER 5.0
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER5}
  {$ENDIF}
{$ENDIF}

// DELPHI 6.0
{$IFDEF VER140}
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI6}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE QUERYEDITOR}
  {$DEFINE USETPARAM}
{$ENDIF}

// DELPHI 7.0
{$IFDEF VER150}
  {$DEFINE KNOWNCOMPILER}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI7}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE USETPARAM}
  {$DEFINE QUERYEDITOR}
{$ENDIF}


// Check if compiler is known and warning if not!
{.$DEFINE KNOWNCOMPILER}
{$IFNDEF KNOWNCOMPILER}
YOU ARE USING AN UNKNOWN COMPILER !
YOU CAN REMOVE THE ERROR MESSAGE BY REMOVING THE DOT (.) ON THE LINE
"{.$DEFINE KNOWNCOMPILER}"  ABOVE

YOU HAVE BEEN WARNED !!!

{$ENDIF}
{#Z- show to scanhelp}
{#B+ scanhelp directive}
{#G+ scanhelp directive}
{#F+ scanhelp directive}



