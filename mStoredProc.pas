unit mStoredProc;
{$I mODBC.INC}
{#B+ scanhelp directive}
{#F+ scanhelp directive}

{ Unit mStoredProc !
This contains the #TmStoredProc# component !
It handles Stored Procedures in SQL databases.

If any operation in #TmStoredProc# returns SQL_SUCCESS_WITH_INFO ,
the component will not raise an Exception,
it will trigger an event #TmDataBase.OnSuccessWithInfo#
 

Revision history:

2000-08-15 Per-Eric Larsson
  Rewritten from MStored !

}
interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  mDatabas,
  ODBCsql,
  db,
  mParams;

type
  TBoundParamInfo = class(Tobject)
{#F+ This type is used to keep information about each
parameter for the Stored Procedure,
the ordinary parameter type lacks some information
-especially BufferLength }
    ParameterNumber: SQLUSMALLINT;
    InputOutputType: SQLSMALLINT;
    ValueType_C: SQLSMALLINT;
    ParameterType_SQL: SQLSMALLINT;
    ColumnSize: SQLUINTEGER;
    DecimalDigits: SQLSMALLINT;
    BufferLength: SQLINTEGER;
    ParameterValuePtr: SQLPOINTER;
    StrLen_or_IndPtr: SQLINTEGER;
  end;

type
  TmStoredProc = class(TComponent)
  private
    FDataBase: tmDataBase;
    FPrepared: Boolean;
    FStoredProcName: string;
    Fhstmt: SQLHSTMT;
    FParams: TStParams;
    FBoundParams: TStringList;
    procedure SetDataBase(const Value: tmDataBase);
    procedure SetPrepared(const Value: Boolean);
    function DoPrepare: boolean;
    function DoUnPrepare: boolean;
    procedure DoExecProc;
    procedure SetStoredProcName(const Value: string);
    procedure GetProcParams(const procname: string);
    procedure CheckSQLResult(sqlres: SQLRETURN);
{this CheckSQLResult does not generate an Exception for SQL_SUCCESS_WITH_INFO ,
it triggers an event in the #mDataBase.SuccessWithInfoEvent#}
    function GetSQL: string;
{Creates an SQL string to send to ODBC for invoking the Stored Procedure}
    procedure SetParams(const Value: TStParams);
    function BindParameter(ahstmt: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      BoundInfo: TBoundParamInfo): boolean;
    function FreeBoundParams: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(const Value: string): TstParam;
{Accesses parameter information based on a specified parameter name.
"links" to the #Params#.ParamByName}
    procedure ExecProc;
{#F+
Executes the stored procedure on the server.
Call ExecProc to execute a stored procedure on the server.
Before calling ExecProc:

1	Provide any input parameters in the Params property.
  At design time, a developer can provide parameters using the Parameters editor.
  At runtime an application must access Params directly.
2	Call Prepare to bind the parameters.

If a stored procedure returns output parameters,
they are stored in the Params property when ExecProc returns control to the application.
An application can access the output parameters by indexing into the Params list,
or by using the #ParamByName# method.

Note:	If an application attempts to execute a stored procedure that has not been prepared,
  mODBC automatically prepares the procedure before executing it,
  and then unprepares it when execution is complete.
  If a stored procedure will be executed more than once,
  it is more efficient for an application to call Prepare explicitly once
  to avoid repeated and unnecessary preparing and unpreparing of the stored procedure,
  and then call UnPrepare when the stored procedure is no longer needed.
}
    procedure Prepare;
{Prepares a stored procedure for execution.

Call Prepare to bind a stored procedure’s parameters
before calling #ExecProc# to execute the procedure.
}
    procedure UnPrepare;
{
Frees the resources allocated for a previously prepared stored procedure.

Call UnPrepare to free the resources allocated for a
previously prepared stored procedure on the server and client sides.
}
    property Prepared: Boolean read FPrepared write SetPrepared;
{
Determines whether or not a stored procedure is prepared for execution.

Examine Prepared to determine if a stored procedure is already prepared for execution.
If Prepared is True, the stored procedure is prepared,
and if Prepared is False, the procedure is not prepared.
A stored procedure must be prepared before it can be executed.

Note:
mODBC automatically prepares a stored procedure if it is unprepared when the application calls #ExecProc#.
After execution, mODBC automatically unprepares the stored procedure.
If a procedure will be executed a number of times,
it is more efficient for the application to prepare the stored procedure once,
and unprepare it when it is no longer needed.

An application can change the current setting of Prepared to prepare or unprepare a stored procedure.
If Prepared is True, setting it to False calls the #Unprepare# method to unprepare the stored procedure.
If Prepared is False,setting it to True calls the #Prepare# method to prepare the procedure.
Generally, however, it is better programming practice to call Prepare and Unprepare directly.
These methods automatically update the Prepared property.}
  published
    { Published declarations }
    property DataBase: tmDataBase read FDataBase write SetDataBase;
{The #tmDataBase# component we are attached thru}
    property StoredProcName: string read FStoredProcName write SetStoredProcName;
{Identifies the name of the stored procedure on the server for which this object is an encapsulation.}
    property Params: TStParams read FParams write SetParams;
{The parameters for the stored procedure we want to call }
  end;


procedure Register;

implementation
uses
  mExcept,
  odbchelper;

procedure Register;
begin
  RegisterComponents('mODBC', [TmStoredProc]);
end;

{ TmStoredProc }

function TmStoredProc.DoPrepare: boolean;
var
  AStatment: string;
  ABoundParamInfo: TBoundParamInfo;
  AparamName: array[0..SQL_NAME_LEN + 1] of char;
  DECIMAL_DIGITS, COLUMN_TYPE, DATA_TYPE: SQLSMALLINT;
  COLUMN_SIZE, BUFFER_LENGTH: integer;
  l: SQLINTEGER; //DWORD;
  Res: SQLRETURN;
  pno: SQLUSMALLINT;
  i: integer;
  h: SQLHSTMT;
begin
// make sure we are connected
//  result := false;
  database.Connect;

  FreeBoundParams;
  CheckSQLResult(SQLAllocHandle(SQL_HANDLE_STMT, DataBase.hdbc, h));

  CheckSQLResult(SQLProcedureColumns(h, nil, 0, nil, 0, pchar(FStoredProcName), length(FStoredProcName), nil, 0));
  pno := 1;
  SQLBindCol(h, 4, SQL_CHAR, @AparamName[0], SQL_NAME_LEN, @l);
  SQLBindCol(h, 5, SQL_SMALLINT, @COLUMN_TYPE, SizeOf(SmallInt), @l);
  SQLBindCol(h, 6, SQL_SMALLINT, @DATA_TYPE, SizeOf(SmallInt), @l);
  SQLBindCol(h, 8, SQL_INTEGER, @BUFFER_LENGTH, SizeOf(integer), @l);
  SQLBindCol(h, 9, SQL_INTEGER, @COLUMN_SIZE, SizeOf(integer), @l);
  SQLBindCol(h, 10, SQL_INTEGER, @DECIMAL_DIGITS, SizeOf(SmallInt), @l);
  Res := SQLFetch(h);
  while (Res = SQL_SUCCESS) do
  begin
    ABoundParamInfo := TBoundParamInfo.Create;
    ABoundParamInfo.ParameterNumber := pno;
    ABoundParamInfo.InputOutputType := COLUMN_TYPE;
    ABoundParamInfo.ParameterType_SQL := DATA_TYPE;
    ABoundParamInfo.ColumnSize := COLUMN_SIZE;
    ABoundParamInfo.DecimalDigits := DECIMAL_DIGITS;
    ABoundParamInfo.BufferLength := BUFFER_LENGTH;
    GetMem(ABoundParamInfo.ParameterValuePtr, ABoundParamInfo.BufferLength);

    FBoundParams.AddObject(strpas(AparamName), ABoundParamInfo);

    Res := SQLFetch(h);
    inc(pno);
  end;
  CheckSQLResult(SQLFreeHandle(SQL_HANDLE_STMT, h));

  AStatment := GetSQL;

  CheckSQLResult(SQLAllocHandle(SQL_HANDLE_STMT, DataBase.hdbc, Fhstmt));
  CheckSQLResult(SQLPrepare(Fhstmt, pchar(AStatment), SQL_NTS));

  for i := 0 to FBoundParams.count - 1 do
  begin
    BindParameter(Fhstmt, i + 1, TBoundParamInfo(FBoundParams.objects[i]));
  end;
  result := true;
end;

function TmStoredProc.BindParameter(ahstmt: SQLHSTMT; ParameterNumber: SQLUSMALLINT; BoundInfo: TBoundParamInfo): boolean;
var
  IOType: SQLSMALLINT;
begin
// result := false;
// must convert "Result" column to "Output" column !
  case BoundInfo.InputOutputType of
    SQL_RETURN_VALUE: IOType := SQL_PARAM_OUTPUT;
  else
    IOType := BoundInfo.InputOutputType;
  end;

// convert some odd types to C_DATA_TYPE
  BoundInfo.ValueType_c := SQLtoCDataType(BoundInfo.ParameterType_SQL);

// If string then set Length pointer to "Null-terminated"
  case BoundInfo.ValueType_c of
    SQL_CHAR: BoundInfo.StrLen_or_IndPtr := SQL_NTS;
  else
    BoundInfo.StrLen_or_IndPtr := BoundInfo.BufferLength;
  end;

// Bind it !
  CheckSQLResult(
    SQLBindParameter(ahstmt, ParameterNumber, IOType,
    BoundInfo.ValueType_c, BoundInfo.ParameterType_SQL,
    BoundInfo.ColumnSize, BoundInfo.DecimalDigits,
    BoundInfo.ParameterValuePtr, 0,
    @BoundInfo.StrLen_or_IndPtr));
  result := true;
end;

procedure TmStoredProc.CheckSQLResult(sqlres: SQLRETURN);
begin
  case sqlres of
    SQL_SUCCESS: exit;
    SQL_SUCCESS_WITH_INFO: Database.GetSuccessInfo(SQL_HANDLE_STMT, Fhstmt);
  else
    raise ESQLerror.CreateDiag(SQL_HANDLE_STMT, Fhstmt, sqlres);
  end;
end;

function TmStoredProc.DoUnPrepare: boolean;
begin
//  result := false;
  CheckSQLResult(SQLFreeStmt(Fhstmt, SQL_UNBIND));
  CheckSQLResult(SQLFreeHandle(SQL_HANDLE_STMT, Fhstmt));
  result := FreeBoundParams;
end;

procedure TmStoredProc.DoExecProc;
var
  kc: SQLRETURN;
  i: integer;
  ABoundParamInfo: TBoundParamInfo;
begin
//  Move data from parameter values to Bound pointers
  for i := 0 to FBoundParams.count - 1 do
  begin
    ABoundParamInfo := TBoundParamInfo(FBoundParams.objects[i]);
    params.items[i].GetData(ABoundParamInfo.ParameterValuePtr);
  end;

// Execute the procedure
  CheckSQLResult(SQLExecute(Fhstmt));

(*
From ODBC.hlp :
After the statement has been executed,
drivers store the returned values of input/output and output parameters
in the variables bound to those parameters.
Note that these are not guaranteed to be set
until after all results returned by the procedure
have been fetched and SQLMoreResults has returned SQL_NO_DATA.*)

  kc := SQLMoreResults(Fhstmt);
  while (kc <> SQL_NO_DATA) do
  begin
    SQLFetch(Fhstmt);
    kc := SQLMoreResults(Fhstmt);
  end;

//  Move data from Bound pointers to parameter values
  for i := 0 to FBoundParams.count - 1 do
  begin
    ABoundParamInfo := TBoundParamInfo(FBoundParams.objects[i]);
    params.items[i].SetData(ABoundParamInfo.ParameterValuePtr);
  end;

end;

function TmStoredProc.FreeBoundParams: boolean;
var
  ABoundParamInfo: TBoundParamInfo;
  i: integer;
begin
  for i := 0 to FBoundParams.count - 1 do
  begin
    ABoundParamInfo := TBoundParamInfo(FBoundParams.objects[i]);
    FreeMem(ABoundParamInfo.ParameterValuePtr, ABoundParamInfo.BufferLength);
  end;
  FBoundParams.clear;
  result := true;
end;

procedure TmStoredProc.ExecProc;
begin
  if Prepared then
  begin
    DoExecProc;
  end
  else
  begin
    if DoPrepare then
    begin
      DoExecProc;
      DoUnPrepare;
    end;
  end;
end;

procedure TmStoredProc.Prepare;
begin
  if not Fprepared then
  begin
    Prepared := DoPrepare;
  end;
end;

procedure TmStoredProc.UnPrepare;
begin
  if Fprepared then
  begin
    Prepared := not DoUnPrepare;
  end;
end;

procedure TmStoredProc.SetDataBase(const Value: tmDataBase);
begin
  FDataBase := Value;
end;

procedure TmStoredProc.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;

procedure TmStoredProc.SetStoredProcName(const Value: string);
begin
  if FStoredProcName <> Value then
  begin
    if not (csLoading in ComponentState) then
    begin
      GetProcParams(Value);
    end;
  end;
  FStoredProcName := value;
end;

procedure TmStoredProc.GetProcParams(const procname: string);
var
  h: SQLHANDLE;
  AparamName: array[0..SQL_NAME_LEN + 1] of char;
  COLUMN_TYPE, DATA_TYPE: SQLSMALLINT;
  l: SQLINTEGER; //DWORD;
  Res: SQLRETURN;
  AParam: TstParam;
begin
  if assigned(FParams) then
  begin
    FParams.Clear;
    database.Connect;
    if SQLAllocHandle(SQL_HANDLE_STMT, database.hdbc, h) = SQL_SUCCESS then
    begin
      try
        Res := SQLProcedureColumns(h, nil, 0, nil, 0, pchar(procname), length(procname), nil, 0);
        if Res = SQL_SUCCESS then
        begin
          SQLBindCol(h, 4, SQL_CHAR, @AparamName[0], SQL_NAME_LEN, @l);
          SQLBindCol(h, 5, SQL_SMALLINT, @COLUMN_TYPE, SizeOf(SmallInt), @l);
          SQLBindCol(h, 6, SQL_SMALLINT, @DATA_TYPE, SizeOf(SmallInt), @l);
          Res := SQLFetch(h);
          while (Res = SQL_SUCCESS) do
          begin
            aParam := Tstparam.Create(fparams, SQLParamTypeToParamType(COLUMN_TYPE));
            aParam.ParamType := SQLParamTypeToParamType(COLUMN_TYPE);
            aParam.DataType := SQLdatatypeTOBDEFieldType(DATA_TYPE);
            aParam.Name := AparamName;
            Res := SQLFetch(h);
          end;
        end;
      finally
        SQLFreeHandle(SQL_HANDLE_STMT, h);
      end;
    end;
  end;
end;


function TmStoredProc.GetSQL: string;
var
  before, after: string;
  i: integer;
begin
  before := ' call "' + FStoredProcName + '" (';
  after := '';
  begin
    for i := 0 to Params.Count - 1 do
    begin
      case TstParam(Params.items[i]).ParamType of
        ptResult:
          begin
            before := ' ?= ' + before;
          end;
      else
        begin
          if after = '' then
          begin
            after := '?';
          end
          else
          begin
            after := after + ',?';
          end;
        end;
      end;
    end;
  end;
  result := '{ ' + before + after + ' ) } ';
end;

procedure TmStoredProc.SetParams(const Value: TstParams);
begin
  FParams := Value;
end;

constructor TmStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF USETPARAM}
  FParams := TstParams.Create(Self);
{$ELSE}
  FParams := TstParams.Create;
{$ENDIF}
  FBoundParams := TStringList.Create;
end;

destructor TmStoredProc.Destroy;
begin
  UnPrepare;
  Destroying;
  FreeBoundParams;
  FBoundParams.Free;
  FParams.Free;
  inherited Destroy;
end;

function TmStoredProc.ParamByName(const Value: string): TstParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TmStoredProc.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataBase) then
    FDataBase := nil;
end;

end.

