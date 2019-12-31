unit mHstmt;

{$I mODBC.INC}

{$RANGECHECKS OFF}

interface

uses classes, dbconsts,
     mDataBas, odbcsql, mSQLParm,
     mExcept, mConst;

const
  mMaxBufferCount = MAXINT DIV 8;
type

  PWord = ^Word;
  PByte     =^Byte;
{$IFNDEF D4UP}
// Declared in Delphi 4+
  PDateTime =^TDateTime;
{$ENDIF}


  // prepare -> execute -> fetching -> close

  TSQLFieldDef = record
     FieldNo:     Integer;
     Name:        String;
     Offset:      integer;     // offset in record buffer
     Precision:   integer;     // precision for double fields
     Required:     boolean;
     SQLDataType: SQLSMALLINT; // ODBC data type
     SQLSize:     SQLUINTEGER; // size of buffer
  end;

  TSQLFieldDefs = array [0..0] of TSQLFieldDef;

  TFetchStatus = ( fsBOF, fsEOF, fsCurrent);
  TmGetMode = ( mgFirst, mgNext, mgPrior, mgLast,mgCurrent);
  TmfStatus = ( mfOk, mfBOF, mfEOF, mfError);
  TmHstmtStateBit = (mhOpened, mhPrepared, mhInitialized);
  TmHstmtState = set of TmHstmtStateBit;
  TmCursorTypes = (ctFwdOnlyCursor,ctStaticCursor,ctKeySetCursor,ctDynamicCursor);

  TmCursorAttributes = record
    Attrib1:           SQLUINTEGER;
    Attrib2:           SQLUINTEGER;
    UseDriverBookMark: Boolean;
    Concurrency:       SQLUINTEGER;
    Sensitivity:       SQLUINTEGER;
  end;

  TmRecInfo = record
    RecordNumber: SQLINTEGER;
//    UpdateStatus: TUpdateStatus;
//    BookmarkFlag: TBookmarkFlag;
  end;

  PmRecInfo = ^TmRecInfo;
  PBookMarkList = ^TBookMarkList;
  TBookMarkList = array[0..mMaxBufferCount - 1] of Char;
  PmBufferList = ^TmBufferList;
  TmBufferList = array[0..mMaxBufferCount - 1] of PChar;

  TmHstmt = Class(TComponent)
  private
    BookMarkSize:  Integer;
    hstmt:         SQLHSTMT;
    FBindBuffer:   PChar; // buffer for SQLbindColumn
    FBlobCount:    Integer;
    FBlobCacheOfs: Integer;
    FCachedCount:  Integer;            // count of cached records
    FCachedBuffer: PmBufferList;        // buffers with cached records
    FDeletedCount: Integer;            // count of deleted records
    FDeletedRec:   PBookMarkList;      // bookmarks of deleted records
    FetchStatus:   TFetchStatus;
    FParams:       TmSQLParams;
    FRecInfoOfs:   Integer;
    FRecordSize:   Word;
    FState:        TmHstmtState;
    FSQL:          TStrings;
    FSQLFieldDefs: ^TSQLFieldDefs;
    FSQLText:      String;
    FFieldDefcount:SQLSMALLINT;
    FCursorAttr:   TmCursorAttributes;
    FCursorType:   TmCursorTypes;      // type cursor (static,dynamic)
    FDataBase:     TmDataBase;
    FRecBufSize:   Word;
    function AllocRecordBuffer: PChar;
    function BindParameters:PCHAR;
    function CheckDeleted( Buffer: Pointer):boolean;
    function GetCursorName: String;
    function GetPrepared: Boolean;
    function GetActive: boolean;
    function getBookmarkPtr(Buffer: Pointer):Pointer;
    procedure CheckSQLResult(sqlres: SQLRETURN);
    procedure CopyRecordBuffer( BufferSrc, BufferDst:PChar);
    procedure CreateParams(List: TmSQLParams; const Value: PChar);
    procedure FreeRecordBuffer(var Buffer: PChar);
    procedure FreeStmt;
    procedure GetStmtAttributes;
    procedure InitStmt;
    procedure SetActive(const Value: boolean);
    procedure SetCursorType(const Value: TmCursorTypes);
    procedure SetDataBase(const Value: TmDataBase);
    procedure SQLBindColumns;
    procedure SQLUnBindColumns;
    procedure QueryChanged(Sender: TObject);
    procedure UnBindParameters(Buffer:Pchar);
//    function GetFieldDataByIndex(FieldNo: Integer): Variant;
    function  CheckGetModeAndScroll( const GetMode: TmGetMode): SQLRETURN;
    function  CheckSqlResInFetch( const sqlres: SQLRETURN; const GetMode: TmGetMode): TmfStatus;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Fetch( GetMode:TmGetMode):TmfStatus;
    function GetFieldData( FieldName:String; Buffer: Pointer):boolean;
    procedure Close;
    procedure Execute;
    procedure Prepare;
    procedure UnPrepare;
    property  CursorAttr: TmCursorAttributes read FCursorAttr;
    property  CursorName:String read GetCursorName;
    property  Prepared: Boolean read GetPrepared;
  published
    property Active:boolean read GetActive write SetActive;
    property CursorType:TmCursorTypes read FCursorType write SetCursorType default ctDynamicCursor;
    property DataBase:TmDataBase read FDataBase write SetDataBase;
    property SQL:Tstrings read FSQL write FSQL;
  end;

implementation

uses SysUtils,odbchelper;

{$include mHstmtHelper.pas}


{ TmHstmt }

constructor TmHstmt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL           := TStringList.Create;
  TStringList(FSQL).OnChange := QueryChanged;
  hstmt          := 0;
  FCursorType    := ctDynamicCursor;
  FState         := [];
  FFieldDefcount := 0;
  FSQLFieldDefs  := nil;
  FParams        := TmSQLParams.Create;
end;

destructor TmHstmt.Destroy;
begin
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TmHstmt.AllocRecordBuffer: PChar;
begin
  Result := StrAlloc(FRecBufSize);
  if FBlobCount > 0 then
    Initialize(PmBlobDataArray(Result + FBlobCacheOfs)[0], FBlobCount);
end;

procedure TmHstmt.CheckSQLResult( sqlres:SQLRETURN);
begin
  case sqlres of
    SQL_SUCCESS:;
    SQL_SUCCESS_WITH_INFO: raise ESQLerror.CreateDiag( SQL_HANDLE_STMT, hstmt, sqlres);
    SQL_NEED_DATA:         raise ESQLerror.Create( 'SQL_NEED_DATA');
    SQL_STILL_EXECUTING:   raise ESQLerror.Create( 'SQL_STILL_EXECUTING');
    SQL_ERROR:             raise ESQLerror.CreateDiag( SQL_HANDLE_STMT,hstmt, sqlres);
    SQL_NO_DATA:           raise ESQLerror.Create( 'SQL_NO_DATA');
    SQL_INVALID_HANDLE:    raise ESQLerror.Create( 'SQL_INVALID_HANDLE');
    else                   raise ESQLerror.Create( 'unknown SQL result');
  end;
end;

procedure TmHstmt.Close;
begin
  SQLFreeStmt( hstmt, SQL_CLOSE); { SQLCloseCursor( hstmt); }
  Exclude(FState, mhOpened);
  SQLUnBindColumns;

  if FFieldDefcount > 0 then
  begin
    FreeMem(FSQLFieldDefs);
    FFieldDefcount := 0;
  end;

  if FBindBuffer<>nil then
  begin
    FreeRecordBuffer( FBindBuffer);
    FBindBuffer := nil;
  end;
end;

function TmHstmt.CheckDeleted( Buffer: Pointer):boolean;
var
  i: Integer;
begin
  Result := False;
  if (FCursorAttr.Sensitivity and SQL_SS_DELETIONS) = 0 then
  begin
    for i := 0 to FDeletedCount-1 do
      if MemCompare(getBookmarkPtr( Buffer), @(FDeletedRec[i*BookMarkSize]), BookMarkSize)
        then Exit;
  end;
  Result := True;
end;

procedure TmHstmt.CopyRecordBuffer( BufferSrc, BufferDst: PChar);
var
  i: LongInt;
begin
  Move( BufferSrc^, BufferDst^, FBlobCacheOfs);

  for i := 0 to FBlobCount-1 do
  begin
    PmBlobDataArray( BufferDst + FBlobCacheOfs)[i] :=
          PmBlobDataArray( BufferSrc + FBlobCacheOfs)[i];
  end;

  Move( (BufferSrc+FRecInfoOfs)^,
        (BufferDst+FRecInfoOfs)^, SizeOf(TmRecInfo));
end;

function TmHstmt.CheckGetModeAndScroll( const GetMode: TmGetMode): SQLRETURN;
begin
  case GetMode of
    mgCurrent:
      case FetchStatus of
        fsBOF: result := SQLFetchScroll( hstmt, SQL_FETCH_FIRST, 0);
        fsEOF: result := SQLFetchScroll( hstmt, SQL_FETCH_LAST, 0);
        else   result := SQLFetchScroll( hstmt, SQL_FETCH_RELATIVE, 0);
      end;
//      sqlres := SQLSetPos( hstmt, 1, SQL_POSITION, SQL_LOCK_NO_CHANGE);
    mgNext:
      case FetchStatus of
        fsBOF: result := SQLFetchScroll( hstmt, SQL_FETCH_FIRST, 0);
        fsEOF: result := SQL_ERROR;
        else   result := SQLFetchScroll( hstmt, SQL_FETCH_NEXT, 1);
      end;
//      sqlres := SQLExtendedFetch( hstmt, SQL_FETCH_NEXT, 1, @rCount, @rStatus);
    mgPrior:
      case FetchStatus of
        fsBOF: result := SQL_ERROR;
        fsEOF: result := SQLFetchScroll( hstmt, SQL_FETCH_LAST, 0);
        else   result := SQLFetchScroll( hstmt, SQL_FETCH_PRIOR, 1);
      end;
    else
      result := SQL_ERROR;
  end;
end;

function TmHstmt.CheckSqlResInFetch( const sqlres: SQLRETURN;
                                     const GetMode: TmGetMode): TmfStatus;
begin
  Result := mfOk;

  case sqlres of
    SQL_SUCCESS:;
    SQL_SUCCESS_WITH_INFO:
      try
        checkSQLResult(sqlres);
      except on E: ESQLerror do
        if (E.SqlState <> '01S06') and (GetMode = mgPrior)
          then Result := mfBOF
          else raise;
      end;
    SQL_NEED_DATA,
    SQL_STILL_EXECUTING,
    SQL_ERROR,
    SQL_INVALID_HANDLE:
      begin
        checkSQLResult(sqlres);
        Result := mfError;
      end;
    SQL_NO_DATA:
      begin
        if GetMode = mgNext then
          Result := mfEOF
        else
          if GetMode = mgPrior then
            Result := mfBOF
          else Result := mfError;
      end;
     else Result := mfError;
  end;
end;


function TmHstmt.Fetch( GetMode: TmGetMode): TmfStatus;
var
  sqlres: SQLRETURN;
  i: integer;
  bind: SQLINTEGER;
  c:   AnsiString;
  iv:  SQLINTEGER;
  b:   LongInt;
  label TryAgain;
begin
  SQLBindColumns;

  TryAgain:

  sqlres := CheckGetModeAndScroll( GetMode);

  FetchStatus := fsCurrent;

  result := CheckSqlResInFetch( sqlres, GetMode);

  if Result <> mfOk then
    exit;

  if CursorAttr.UseDriverBookMark // read bookmark
    then CheckSQLResult( SQLGetData( hstmt, 0, SQL_BINARY,
                                  SQLPOINTER(FBindBuffer),BookMarkSize,@bind));

  with PmRecInfo(FBindBuffer + FRecInfoOfs)^ do
  begin
    //BookmarkFlag := fsCurrent;
    if SQLGetStmtAttr( hstmt, SQL_ATTR_ROW_NUMBER, @RecordNumber, SQL_IS_UINTEGER, nil)
       <> SQL_SUCCESS then
    begin
      RecordNumber := -1;
    end;
    if RecordNumber = 0 then
      RecordNumber := -1;
  end;

  // check deleted and filtered records
  if not CheckDeleted(FBindBuffer) then
  begin
    if GetMode = mgCurrent then
      GetMode := mgNext;
    goto TryAgain;
  end;

  try
    // check if record cached
    b := -1;

    for i := 0 to FCachedCount-1 do
    begin
      if MemCompare( getBookmarkPtr( FBindBuffer),
                     getBookmarkPtr( FCachedBuffer[i]), BookMarkSize) then
      begin
        b := i;
        break;
      end;
    end;

    if b >= 0 then
    begin // read fields into buffer from cache
      CopyRecordBuffer( FCachedBuffer[b], FBindBuffer);
    end else
    begin// read fields into buffer from driver
       {if CursorAttr.UseDriverBookMark
            then Move( (FBindBuffer+BookMarkSize)^,
                       (Buffer+BookMarkSize)^, FRecordSize-BookMarkSize) // copy only binded fields without blob
            else Move( FBindBuffer^, Buffer^, FRecordSize); // copy only binded fields without blob }
      for I := 0 to FFieldDefcount - 1 do
      with FSQLFieldDefs[I] do
      begin
        if SQLDataType = SQL_BINARY then {
        CheckSQLResult( SQLGetData( hstmt,FieldNo, SQLDataType,
                                    SQLPOINTER(Buffer+OffsetInBuf),SqlSize,
                                    PSQLINTEGER(Buffer+OffsetInBuf+SqlSize)))
        else }
        begin
          // load blob fields
          c := '';
          sqlres := SQLGetData( hstmt, FieldNo, SQL_BINARY,
                                SQLPOINTER( @i), 0, @iv); // get data length
           if (sqlres = SQL_SUCCESS)
             or(sqlres = SQL_SUCCESS_WITH_INFO) then
           begin
             SetLength( c, iv+1); // one more byte for #0
             sqlres := SQLGetData( hstmt, FieldNo, SQL_BINARY,
                                   SQLPOINTER( PCHAR(c)), iv+1, @iv);
             if (sqlres <> SQL_SUCCESS) and (sqlres <> SQL_NO_DATA) then
               CheckSQLResult( sqlres);

             SetLength( c, iv); // set length to real getted data
           end;
           PmBlobDataArray(FBindBuffer + FBlobCacheOfs)[Offset]:=c;
        end;
      end;
    end;
  except
    Result := mfError;
  end;
{    if not CheckFilter(Buffer) then
  begin
       if GetMode=gmCurrent then GetMode:=gmNext;
       goto TryAgain;
  end; }
end;

procedure TmHstmt.FreeRecordBuffer(var Buffer: PChar);
begin
  if FBlobCount > 0 then
    Finalize(PmBlobDataArray(Buffer + FBlobCacheOfs)[0], FBlobCount);

  StrDispose(Buffer);
end;

function TmHstmt.getBookmarkPtr(Buffer: Pointer):Pointer;
begin
  if CursorAttr.UseDriverBookMark
    then Result := Buffer
    else Result := @(PmRecInfo(PChar(Buffer) + FRecInfoOfs).RecordNumber);
end;

function TmHstmt.GetFieldData( FieldName:String; Buffer: Pointer):boolean;

  function FieldDefIndex( fname: string): integer;
  var i: integer;
  begin
    for I := 0 to FFieldDefcount - 1 do
      with FSQLFieldDefs[I] do
        if Name = FieldName then
        begin
          Result := i;
          exit;
        end;
    result := -1;
  end;

var
  i,
  index: integer;
  fd:    ^TSQLFieldDef;
//  b:     ^TDateTimeRec;
begin
  index := FieldDefIndex(FieldName);
  if index < 0 then
    raise Exception.create('field not founded');

  Result := False;

  fd := @(FSQLFieldDefs[index]);

  if PSQLINTEGER(FBindBuffer + fd.offset + fd.SqlSize)^ = SQL_NULL_DATA then
    Exit;

  if Buffer <> nil then
  case fd.SQLDataType of
    SQL_CHAR:
      begin
        Move( (FBindBuffer + fd.Offset)^, Buffer^, fd.SqlSize);
        i := StrLen( Buffer)-1;
        while (i >= 0) and (PChar(Buffer)[i]=#32) do
        begin
          PChar(Buffer)[i] := #0;
          dec(i);
        end;
      end;
    SQL_SMALLINT,
    SQL_INTEGER,
    SQL_DOUBLE:
      begin
        Move( (FBindBuffer+fd.Offset)^, Buffer^, fd.SqlSize);
      end;
    SQL_BIT:
      begin
        PWord(Buffer)^ := PByte( FBindBuffer + fd.Offset)^;
      end;
    SQL_TYPE_DATE:
      begin
        with PSQL_DATE_STRUCT( FBindBuffer+fd.Offset)^ do
          PDateTime(Buffer)^:= EncodeDate( Year, Month, Day);
      end;
    SQL_TYPE_TIME:
      begin
        with PSQL_TIME_STRUCT( FBindBuffer+fd.Offset)^ do
          PDateTime(Buffer)^:= EncodeTime( Hour, Minute, Second, 0);
      end;
    SQL_TYPE_TIMESTAMP:
      begin
        with PSQL_TIMESTAMP_STRUCT( FBindBuffer + fd.Offset)^ do
          PDateTime(Buffer)^:= EncodeDate( Year, Month, Day)+
                               EncodeTime( Hour, Minute, Second, 0);
      end;
    else
      raise Exception.CreateFmt( SmFieldUnsupportedType,[ FieldName]);
  end; // case

  Result := True;
end;

procedure TmHstmt.GetStmtAttributes;
var
  ui: SQLUINTEGER;
begin
  CheckSQLResult( SQLGetStmtAttr( hstmt, SQL_ATTR_CURSOR_TYPE, @ui, sizeof(ui), nil));

  case ui of
    SQL_CURSOR_FORWARD_ONLY:  CursorType:= ctFwdOnlyCursor;
    SQL_CURSOR_STATIC:        CursorType:= ctStaticCursor;
    SQL_CURSOR_KEYSET_DRIVEN: CursorType:= ctKeySetCursor;
    SQL_CURSOR_DYNAMIC:       CursorType:= ctDynamicCursor;
    else raise ESQLerror.Create( 'Unknown cursor type');
  end;

  with FCursorAttr do
  begin
    Attrib1 := 0;
    Attrib2 := 0;
    Sensitivity := 0;
    try
      case CursorType of
        ctFwdOnlyCursor:
          begin
            // none
          end;
        ctStaticCursor:
          begin
            DataBase.ODBCDriverInfo( SQL_STATIC_CURSOR_ATTRIBUTES1, SQLPOINTER(@Attrib1), sizeof(SQLUINTEGER), nil);
            DataBase.ODBCDriverInfo( SQL_STATIC_SENSITIVITY, SQLPOINTER(@Sensitivity), sizeof(SQLUINTEGER), nil);
            //DataBase.ODBCDriverInfo( SQL_STATIC_CURSOR_ATTRIBUTES2, SQLPOINTER(@Attrib2), sizeof(SQLUINTEGER), nil);
          end;
        ctKeySetCursor:
          begin
            DataBase.ODBCDriverInfo( SQL_KEYSET_CURSOR_ATTRIBUTES1, SQLPOINTER(@Attrib1), sizeof(SQLUINTEGER), nil);
            DataBase.ODBCDriverInfo( SQL_STATIC_SENSITIVITY, SQLPOINTER(@Sensitivity), sizeof(SQLUINTEGER), nil);
            //DataBase.ODBCDriverInfo( SQL_KEYSET_CURSOR_ATTRIBUTES2, SQLPOINTER(@Attrib2), sizeof(SQLUINTEGER), nil);
          end;
        ctDynamicCursor:
          begin
            DataBase.ODBCDriverInfo( SQL_DYNAMIC_CURSOR_ATTRIBUTES1, SQLPOINTER(@Attrib1), sizeof(SQLUINTEGER), nil);
            Sensitivity:= SQL_SS_ADDITIONS + SQL_SS_DELETIONS + SQL_SS_UPDATES; // by default
            //DataBase.ODBCDriverInfo( SQL_DYNAMIC_CURSOR_ATTRIBUTES2, SQLPOINTER(@Attrib2), sizeof(SQLUINTEGER), nil);
          end;
      end;
    except;
    end; // some drivers not support second attributes

    DataBase.ODBCDriverInfo( SQL_BOOKMARK_PERSISTENCE, SQLPOINTER(@ui), sizeof(SQLUINTEGER), nil);

    if ((Sensitivity and (SQL_SS_ADDITIONS + SQL_SS_DELETIONS))=0) or (ui=0)
      then UseDriverBookMark := False
      else UseDriverBookMark := True;

    if UseDriverBookMark
      then CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_USE_BOOKMARKS, Pointer(SQL_UB_VARIABLE{ON}), 0))
      else SQLSetStmtAttr( hstmt, SQL_ATTR_USE_BOOKMARKS, Pointer(SQL_UB_OFF), 0);

    CheckSQLResult( SQLGetStmtAttr( hstmt, SQL_ATTR_CONCURRENCY, @Concurrency, sizeof(Concurrency), nil));
  end;
end;

procedure TmHstmt.InitStmt;
Var
   sqlres: SQLRETURN;
   ui:     SQLUINTEGER;
begin
  if (mhInitialized in FState) and (hstmt <> 0) then
    Exit;

  if DataBase = nil then
    raise ESQLerror.Create( SmNoDataSource);

  if DataBase.hdbc = 0 then
    DataBase.Connect;

//   DataBase.ODBCDriverInfo( SQL_SCROLL_OPTIONS, SQLPOINTER(@ui), sizeof(SQLUINTEGER), nil);
//   if ui=SQL_SO_FORWARD_ONLY
//          then raise ESQLerror.Create( SmForwardNotSupported);
  sqlres := SQLAllocHandle(SQL_HANDLE_STMT, DataBase.hdbc, hstmt);
  if sqlres <> SQL_SUCCESS then
    raise ESQLerror.Create( SmAllocateSTMTError);

  Include(FState, mhInitialized);
  try
    with FCursorAttr do
    begin

      try
        case CursorType of
          ctFwdOnlyCursor: CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_FORWARD_ONLY), SQL_IS_UINTEGER));
          ctStaticCursor:  CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_STATIC), SQL_IS_UINTEGER));
          ctKeySetCursor:  CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_KEYSET_DRIVEN), SQL_IS_UINTEGER));
          ctDynamicCursor: CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_DYNAMIC), SQL_IS_UINTEGER));
        end;
      except on E: ESQLerror do
        if (E.SqlState <> '01S02'){and(E.SqlState<>'HYC00')} then
          raise;
      end;

      try
        DataBase.ODBCDriverInfo( SQL_SCROLL_CONCURRENCY, SQLPOINTER(@ui), sizeof(SQLUINTEGER), nil);
         if (ui and SQL_SCCO_LOCK) <> 0 then
          CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CONCURRENCY, Pointer(SQL_CONCUR_LOCK), SQL_IS_UINTEGER))
        else
          if (ui and SQL_SCCO_OPT_ROWVER) <> 0 then
            CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CONCURRENCY, Pointer(SQL_CONCUR_ROWVER), SQL_IS_UINTEGER))
          else
            if (ui and SQL_SCCO_OPT_VALUES) <> 0 then
              CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CONCURRENCY, Pointer(SQL_CONCUR_VALUES), SQL_IS_UINTEGER))
            else
              if (ui and SQL_SCCO_READ_ONLY) <> 0 then
                CheckSQLResult( SQLSetStmtAttr( hstmt, SQL_ATTR_CONCURRENCY, Pointer(SQL_CONCUR_READ_ONLY), SQL_IS_UINTEGER));
      except on E: ESQLerror do
        if (E.SqlState <> '01S02'){and(E.SqlState<>'HYC00')} then
          raise;
      end;

      GetStmtAttributes;
    end;
  except
    FreeStmt;
    raise;
  end;
end;

procedure TmHstmt.FreeStmt;
begin
  if hstmt = 0 then
    exit;

  Close;
  SQLFreeHandle(SQL_HANDLE_STMT,hstmt);
  hstmt := 0;
  FState := [];
end;

procedure TmHstmt.Execute;

  // this procedure should be moved out from here
  procedure AddODBCFieldDesc( FFieldNo:Word);
  var
    FName:        string;
    pname:       array [0..255] of char;
    nlength:     SQLSMALLINT;
    dtype:       SQLSMALLINT;
    BufDataType: SQLSMALLINT;
    csize:       SQLUINTEGER;
    decdig:      SQLSMALLINT;
    Nullable:    SQLSMALLINT;
//     i:           Integer;
   begin
     CheckSQLResult( SQLDescribeCol(hstmt,FFieldNo,pname,254,nlength,dtype,csize,decdig,Nullable));
     if FFieldNo = 0 then
     begin
       BookmarkSize := cSize;
       FRecordSize  := cSize;
       exit;
     end;
//    i := 0;
     if pname[0] = #0 then
       StrCopy( pname, 'COLUMN');

     FName := StrPas(pname);

     {     while FieldDefs.IndexOf(FName) >= 0 do
      begin
         Inc( i );
         FName:=Format('%s_%d', [StrPas(pname), i]);
      end; }

     BufDataType := dtype;

     case dtype of
       SQL_CHAR,
       SQL_VARCHAR:
         begin
           BufDataType := SQL_CHAR;
           inc( cSize);
         end;
       SQL_TINYINT,
       SQL_SMALLINT:
         begin
           BufDataType := SQL_SMALLINT;
           cSize := sizeof(SQLSMALLINT);
         end;
       SQL_INTEGER:
         begin
           cSize := sizeof(SQLINTEGER);
         end;
       SQL_DECIMAL,
       SQL_NUMERIC,
       SQL_REAL,
       SQL_FLOAT,
       SQL_DOUBLE:
         begin
           BufDataType := SQL_DOUBLE;
           cSize := sizeof(SQLDOUBLE);
         end;
       SQL_DATE,
       SQL_TYPE_DATE:
         begin
           cSize := sizeof(SQL_DATE_STRUCT);
         end;
       SQL_TIME,
       SQL_TYPE_TIME:
         begin
           cSize := sizeof(SQL_TIME_STRUCT);
         end;
       SQL_TYPE_TIMESTAMP,
       SQL_TIMESTAMP:
         begin
           cSize := sizeof(SQL_TIMESTAMP_STRUCT);
          end;
       SQL_BIT:
         begin
           cSize := sizeof(SQLCHAR);
         end;
       //SQL_BIGINT:     DataType:=ftUnknown;
       SQL_BINARY,
       SQL_VARBINARY,
       SQL_LONGVARBINARY,
       SQL_LONGVARCHAR:
         begin
           BufDataType := SQL_BINARY;
           cSize := 0;
         end;
       else
         BufDataType := SQL_UNKNOWN_TYPE;
     end; // case

     if BufDataType <> SQL_UNKNOWN_TYPE then
     begin
       with FSQLFieldDefs^[FFieldNo] do
       begin
         FieldNo := FFieldNo;
         Name := FName;
         if BufDataType <> SQL_BINARY then
         begin
           Offset := FRecordSize;
         end else
         begin
           Offset := FBlobCount;
           inc( FblobCount);
         end;
         Required := (Nullable = SQL_NO_NULLS);
         SQLDataType := BufDataType;
         SQLsize     := cSize;
         FRecordSize := FRecordSize + cSize + sizeof( SQLINTEGER);
         if SQLDataType = SQL_DOUBLE then
           Precision := decdig;
       end;
     end;
  end;

var
  Colmns: SQLSMALLINT;
  i:      Integer;
  Buffer: Pchar;
begin
  Prepare;

  Buffer := BindParameters;
  try
    try
      CheckSQLResult( SQLExecute( hstmt));
    except
      on E: ESQLerror do
         // change cursor type according stmt attributes
        if E.SqlState = '01S02'
          then GetStmtAttributes
          else raise;
    end;
    Include( FState, mhOpened);
  finally
    UnBindParameters(Buffer);
  end;

   // create field definitions
  CheckSQLResult( SQLNumResultCols( hstmt, Colmns));
  if FFieldDefcount > 0 then
    FreeMem(FSQLFieldDefs);

  FFieldDefcount := Colmns;
  GetMem( FSQLFieldDefs, sizeof(TSQLFieldDefs) * (FFieldDefcount + 1));

  BookmarkSize := sizeof(SQLUINTEGER);
  FRecordSize  := 0;
  FBlobCount   := 0;

  if CursorAttr.UseDriverBookMark then
    for i := 0 to Colmns do
      AddODBCFieldDesc(i)
  else
    for i := 1 to Colmns do
      AddODBCFieldDesc(i);

  FBlobCacheOfs := FRecordSize;
  FRecInfoOfs   := FBlobCacheOfs + FBlobCount * SizeOf(Pointer);
  FRecBufSize   := FRecInfoOfs + SizeOf( TmRecInfo);

  FetchStatus := fsBOF;
end;

procedure TmHstmt.Prepare;
begin
  InitStmt;
  if mhPrepared in FState then
    Exit;
  CheckSQLResult( SQLPrepare( hstmt, PCHAR(FSQL.Text), SQL_NTS));
  Include( FState, mhPrepared);
end;

procedure TmHstmt.SQLBindColumns;
var
  i: Integer;
begin
  if FBindBuffer = nil then
    FBindBuffer := AllocRecordBuffer;

  for I := 0 to FFieldDefcount - 1 do
  begin
    with FSQLFieldDefs[i] do
     if SQLDataType <> SQL_BINARY then
       CheckSQLResult( SQLBindCol( hstmt, i, SQLDataType,
                                   SQLPOINTER( FBindBuffer+Offset),SqlSize,
                                   PSQLINTEGER(FBindBuffer+Offset+SqlSize)));
  end;
end;

procedure TmHstmt.SQLUnBindColumns;
var
  i: Integer;
  sqlres: SQLRETURN;
begin
  if FBindBuffer <> nil then
  begin
    sqlres := SQLFreeStmt( hstmt, SQL_UNBIND);

    if (sqlres <> SQL_SUCCESS) and (sqlres <> SQL_INVALID_HANDLE) then
      for I := 0 to FFieldDefcount - 1 do
      begin
        with FSQLFieldDefs[i] do
          if SQLDataType <> SQL_BINARY then
              SQLBindCol( hstmt, i, SqlDataType, nil, SQLSize, nil);
      end;

     FreeRecordBuffer( FBindBuffer);
     FBindBuffer := nil;
  end;
end;

procedure TmHstmt.UnPrepare;
begin
  Exclude(FState, mhPrepared);
end;

function TmHstmt.GetCursorName: String;
begin

end;

function TmHstmt.GetPrepared: Boolean;
begin
  Result := mhPrepared in FState;
end;

function TmHstmt.GetActive: boolean;
begin
  Result := mhOpened in FState;
end;

procedure TmHstmt.SetActive(const Value: boolean);
begin

end;

procedure TmHstmt.SetCursorType(const Value: TmCursorTypes);
begin
  FCursorType := Value;
end;

procedure TmHstmt.SetDataBase(const Value: TmDataBase);
begin
  FDataBase := Value;
end;

procedure TmHstmt.QueryChanged(Sender: TObject);
var
  List: TmSQLParams;
begin
  FSQLText := SQL.Text;
  if not (csLoading in ComponentState) then
  begin
    Close;
    UnPrepare;
    if (csDesigning in ComponentState) then
    begin
      List := TmSQLParams.Create;
      try
        CreateParams(List, PChar(FSQLText));
        List.AssignValues(FParams);
        FParams.Free;
        FParams := List;
      except
        List.Free;
      end;
    end;
//    DataEvent(dePropertyChange, 0);
  end else
    CreateParams(nil, PChar(FSQLText));
end;

function TmHstmt.BindParameters:Pchar;
var
  bufLen,
  dsize:  LongInt;
  //buf,
  bufPos:  Pchar;
  i:       Integer;
  dtype:   SQLSMALLINT;
  snd:     SQLINTEGER;
begin // bind parameters to stmt
  bufLen := 0;

  for i := 0 to FParams.Count-1 do
  begin
    if not FParams[i].isnull then
      case FParams[i].SQLDataType of
        SQL_TYPE_DATE:      bufLen := bufLen + Sizeof( SQL_DATE_STRUCT);
        SQL_TYPE_TIMESTAMP: bufLen := bufLen + Sizeof( SQL_TIMESTAMP_STRUCT);
        else bufLen := bufLen + FParams[i].GetDataSize;
      end;
  end;

  if bufLen > 0
    then GetMem( Result, bufLen)
    else Result := nil;

  bufPos := Result;
  for i := 0 to FParams.Count-1 do
  begin
    dtype := FParams[i].SQLDataType;
    if (not FParams[i].IsNull) then
    begin
      case dtype of
        SQL_TYPE_DATE:
          begin
             dsize := Sizeof(SQL_DATE_STRUCT);
             PSQL_DATE_STRUCT( BufPos)^ :=
                          DateTimeToDateStruct( FParams[i].AsDateTime);
          end;
        SQL_TYPE_TIMESTAMP:
          begin
             dsize := Sizeof(SQL_TIMESTAMP_STRUCT);
             DateTime2timeStampStruct( PSQL_TIMESTAMP_STRUCT( bufPos)^,
                                       FParams[i].AsDateTime);
          end;
        else
          begin
             dsize:=FParams[i].GetDataSize;
             FParams[i].GetData( bufPos);
          end;
      end; // case

      CheckSQLResult( SQLBindParameter( hstmt, i+1, SQL_PARAM_INPUT, SQL_C_DEFAULT{dtype}, dtype, dsize, 0, bufPos, 0, nil));
      bufPos := bufPos + dsize;
    end else
    begin
      snd := SQL_NULL_DATA;
      CheckSQLResult( SQLBindParameter( hstmt, i+1, SQL_PARAM_INPUT, SQL_C_DEFAULT{dtype}, dtype, 0, 0, nil, 0, @snd));
    end;
  end;
end;

procedure TmHstmt.UnBindParameters(buffer:Pchar);
begin
  if buffer = nil then
    exit;

  CheckSQLResult( SQLFreeStmt( hstmt, SQL_RESET_PARAMS));

  if buffer <> nil then
    FreeMem( buffer);
end;


end.
