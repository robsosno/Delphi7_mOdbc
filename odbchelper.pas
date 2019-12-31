unit odbchelper;
{$I mODBC.INC}

interface
uses
  odbcsql,db;

{$IFNDEF D4UP}
// Declared in db.pas in Delphi 4+
Type
  TParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
{$ENDIF}


Function SQLParamTypeToParamType(const SQLParamType: SQLSMALLINT):TParamType;
Function SQLdatatypeTOBDEFieldType(const SQLDATATYPE: SQLSMALLINT):TFieldType;
function BDE2SqlType(aFieldType: TFieldType): SQLSMALLINT;
function Sql2BDEType( aSQLType: SQLSMALLINT): TFieldType;
function DateStructToDateTime(b: PSQL_DATE_STRUCT): TDateTime;
function DateTimeToDateStruct(b: TDateTime): SQL_DATE_STRUCT;
procedure DateTime2TimeStampStruct(var Value: SQL_TIMESTAMP_STRUCT; b: TDateTime);
function FieldTypetoFunctionName(const aFieldType: TFieldType):string;
function SQLtoCDataType(const SQLDATATYPE: SQLSMALLINT):SQLSMALLINT;



implementation
uses
  Sysutils;

function SQLtoCDataType(const SQLDATATYPE: SQLSMALLINT):SQLSMALLINT;
begin
  case SQLDATATYPE of
    SQL_CHAR,
    SQL_WCHAR,
    SQL_VARCHAR,
    SQL_WVARCHAR,
    SQL_LONGVARCHAR,
    SQL_WLONGVARCHAR:
      begin
        result:=SQL_CHAR;
      end;
    SQL_TINYINT,
    SQL_SMALLINT:
      begin
        result := SQL_SMALLINT;
      end;
    SQL_DECIMAL,
    SQL_NUMERIC,
    SQL_REAL,
    SQL_FLOAT,
    SQL_DOUBLE:
      begin
        result := SQL_DOUBLE;
      end;
    SQL_BIGINT:
      begin
        result := SQL_DOUBLE;
      end;
    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY:
      begin
        result := SQL_BINARY;
      end;
    else
    begin
      result:=SQLDATATYPE;
    end;
  end;
end;

Function SQLParamTypeToParamType(const SQLParamType: SQLSMALLINT):TParamType;
begin
  case SQLParamType of
    SQL_PARAM_INPUT : result:= ptInput;
    SQL_PARAM_INPUT_OUTPUT :result:= ptInputOutput;
    SQL_PARAM_OUTPUT :result:= ptOutput;
    SQL_RETURN_VALUE :result:= ptResult;
    else
//      SQL_RESULT_COL /  SQL_PARAM_TYPE_UNKNOWN
      result:= ptUnknown;
  end;
end;

Function SQLdatatypeTOBDEFieldType(const SQLDATATYPE: SQLSMALLINT):TFieldType;
begin
  case SQLDATATYPE of
    SQL_CHAR,
    SQL_VARCHAR       :result :=ftString;
    SQL_SMALLINT      :result :=ftWord;
    SQL_INTEGER       :result :=ftInteger;
    SQL_DATE          :result :=ftDate;
    SQL_TIMESTAMP     :result :=ftDateTime;
    SQL_DOUBLE        :result :=ftFloat;
    SQL_LONGVARCHAR   :result :=ftMemo;
    SQL_LONGVARBINARY :result :=ftBlob;
    else
      result:=ftUnknown;
  end;
end;

function Sql2BDEType( aSQLType: SQLSMALLINT): TFieldType;
begin
  case aSQLType of
    SQL_CHAR,
    SQL_WCHAR,
    SQL_VARCHAR,
    SQL_WVARCHAR: Result := ftString;
    SQL_TINYINT,
    SQL_SMALLINT: Result := ftSmallint;
    SQL_INTEGER:  Result := ftInteger;
    SQL_DECIMAL,
    SQL_NUMERIC,
    SQL_REAL,
    SQL_FLOAT,
    SQL_DOUBLE:    Result := ftFloat;
    SQL_DATE,
    SQL_TYPE_DATE: Result := ftDate;
    SQL_TIME,
    SQL_TYPE_TIME: Result := ftTime;
    SQL_TYPE_TIMESTAMP,
    SQL_TIMESTAMP: Result := ftDateTime;
    SQL_BIT:       Result := ftBoolean;
    SQL_BIGINT:    Result := ftFloat;
    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY:Result := ftBlob;
    SQL_LONGVARCHAR,
    SQL_WLONGVARCHAR: Result := ftMemo;
    else              Result := ftUnknown;
  end;
end;

function BDE2SqlType( aFieldType: TFieldType): SQLSMALLINT;
begin
  case aFieldType of
    ftString:   result := SQL_CHAR;
    ftWord:     result := SQL_SMALLINT;
    ftSmallint: result := SQL_SMALLINT;
    ftInteger,
    ftAutoInc:  result := SQL_INTEGER;
    ftTime:     result := SQL_TIME;
    ftDate:     result := SQL_DATE;
    ftDateTime: result := SQL_TIMESTAMP;
    ftBCD,
    ftCurrency,
    ftFloat:    result := SQL_DOUBLE;
    ftBoolean:  result := SQL_BIT;
    ftMemo:     result := SQL_LONGVARCHAR;
    ftBlob:     result := SQL_LONGVARBINARY;
    else        result := SQL_UNKNOWN_TYPE;
  end;
end;


function FieldTypetoFunctionName(const aFieldType: TFieldType):string;
begin
  case aFieldType of
{$IFDEF D4UP}
    ftFixedChar,
    ftWideString,
{$ENDIF}
    ftString,
    ftMemo,
    ftFmtMemo : result := '.AsString';

{$IFDEF D4UP}
    ftLargeint,
{$ENDIF}
    ftSmallint,
    ftInteger,
    ftWord,
    ftAutoInc: result := '.AsInteger';

    ftBoolean: result := '.AsBoolean';

    ftFloat,
    ftCurrency,
    ftBCD: result := '.AsFloat';

    ftDateTime: result := '.AsDateTime';
    else result := '';
  end;
(*
    ftDate, ftTime,
    ftBytes, ftVarBytes, ftBlob,  ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary,
    ftCursor,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid);
    ftUnknown,
*)
end;

function DateStructToDateTime(b: PSQL_DATE_STRUCT): TDateTime;
begin
  Result := EncodeDate(b^.Year, b^.Month, b^.Day);
end;

function DateTimeToDateStruct(b: TDateTime): SQL_DATE_STRUCT;
var
  y, m, d: Word;
begin
  DecodeDate(b, y, m, d);
  with Result do
  begin
    Year := y;
    Month := m;
    Day := d;
  end;
end;

procedure DateTime2TimeStampStruct(var Value: SQL_TIMESTAMP_STRUCT; b: TDateTime);
var
  w1, w2, w3, w4: Word;
begin
  with Value do
  begin
    DecodeDate(b, w1, w2, w3);
    Year := w1;
    Month := w2;
    Day := w3;
    DecodeTime(b, w1, w2, w3, w4);
    Hour := w1;
    Minute := w2;
    Second := w3;
    fraction := Integer(w4) * 1000000;
  end;
end;


end.
