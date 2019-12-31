unit mSQLParm;
{$I mODBC.INC}

(*
2002-09-14 Per-Eric Larsson
  Changed {$IFDEF}'s to make it compile under Delphi 7

2001-11-06 Per-Eric Larsson
  Added {$IFDEF}'s to make it compile under Delphi 6
*)

interface
uses Classes, odbcsql;

type
  TmBlobData = AnsiString;
  TmBlobDataArray = array[0..0] of TmBlobData;
  PmBlobDataArray = ^TmBlobDataArray;

  BlobParamDesc = Integer;
  TmSQLParams = class;

  TmParamType = (mptUnknown, mptInput, mptOutput, mptInputOutput, mptResult);

{ TmSQLParam }

  TmSQLParam = class(TPersistent)
  private
    FParamList: TmSQLParams;
    FData: Variant;
    FName: string;
    FDataType: SQLSMALLINT;
    FNull: Boolean;
    FBound: Boolean;
    FParamType: TmParamType;
    procedure InitValue;
  protected
    procedure AssignParam(Param: TmSQLParam);
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function IsEqual(Value: TmSQLParam): Boolean;
    function RecBufDataSize: Integer;
    procedure SetAsBlob(Value: TmBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(Value: Double);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsFloat(Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsVariant(Value: Variant);
    procedure SetDataType(Value: SQLSMALLINT);
    procedure SetText(const Value: string);
  public
    constructor Create(AParamList: TmSQLParams; AParamType: TmParamType);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure GetData(Buffer: Pointer);
    function GetDataSize: Integer;
    procedure SetBlobData(Buffer: Pointer; Size: Integer);
    procedure SetData(Buffer: Pointer);
    property AsBlob: TmBlobData read GetAsString write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Double read GetAsFloat write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsMemo: string read GetAsMemo write SetAsMemo;
    property AsString: string read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property Bound: Boolean read FBound write FBound;
    property SQLDataType: SQLSMALLINT read FDataType write SetDataType;
    property IsNull: Boolean read FNull;
    property Name: string read FName write FName;
    property ParamType: TmParamType read FParamType write FParamType;
    property Text: string read GetAsString write SetText;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

{ TmSQLParams }

  TmSQLParams = class(TPersistent)
  private
    FItems: TList;
    function GetParam(Index: Word): TmSQLParam;
    function GetParamValue(const ParamName: string): Variant;
    procedure SetParamValue(const ParamName: string;
      const Value: Variant);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValues(Value: TmSQLParams);
    procedure AddParam(Value: TmSQLParam);
    procedure RemoveParam(Value: TmSQLParam);
    function CreateParam(FldType: SQLSMALLINT; const ParamName: string;
      ParamType: TmParamType): TmSQLParam;
    function Count: Integer;
    procedure Clear;
    procedure GetParamList(List: TList; const ParamNames: string);
    function IsEqual(Value: TmSQLParams): Boolean;
    function ParamByName(const Value: string): TmSQLParam;
    property Items[Index: Word]: TmSQLParam read GetParam; default;
    property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
  end;

implementation
uses SysUtils, graphics,
{$IFDEF D6UP}
    variants,
{$ENDIF}

     mExcept, mConst;

{ TmSQLParams }

constructor TmSQLParams.Create;
begin
  FItems := TList.Create;
end;

destructor TmSQLParams.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TmSQLParams.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TmSQLParams then
  begin
    Clear;
    for I := 0 to TmSQLParams(Source).Count - 1 do
      with TmSQLParam.Create(Self, mptUnknown) do
        Assign(TmSQLParams(Source)[I]);
  end
  else inherited Assign(Source);
end;

procedure TmSQLParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TmSQLParams then TmSQLParams(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TmSQLParams.AssignValues(Value: TmSQLParams);
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Value.Count - 1 do
      if Items[I].Name = Value[J].Name then
      begin
        Items[I].Assign(Value[J]);
        Break;
      end;
end;

procedure TmSQLParams.AddParam(Value: TmSQLParam);
begin
  FItems.Add(Value);
  Value.FParamList := Self;
end;

procedure TmSQLParams.RemoveParam(Value: TmSQLParam);
begin
  FItems.Remove(Value);
  Value.FParamList := nil;
end;

function TmSQLParams.CreateParam(FldType: SQLSMALLINT; const ParamName: string;
  ParamType: TmParamType): TmSQLParam;
begin
  Result := TmSQLParam.Create(Self, ParamType);
  with Result do
  begin
    Name := ParamName;
    SQLDataType :=  FldType;
  end;
end;

function TmSQLParams.Count: Integer;
begin
  Result := FItems.Count;
end;

function TmSQLParams.IsEqual(Value: TmSQLParams): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end
end;

procedure TmSQLParams.Clear;
begin
  while FItems.Count > 0 do TmSQLParam(FItems.Last).Free;
end;

function TmSQLParams.GetParam(Index: Word): TmSQLParam;
begin
  Result := ParamByName(TmSQLParam(FItems[Index]).Name);
end;

function TmSQLParams.ParamByName(const Value: string): TmSQLParam;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Result := FItems[I];
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  raise ESQLerror.CreateFmt( SmParameterNotFound, [Value]);
  Result := nil;
end;

function TmSQLParams.GetParamValue(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TmSQLParam(Params[I]).Value;
    finally
      Params.Free;
    end;
  end else
    Result := ParamByName(ParamName).Value
end;

procedure TmSQLParams.SetParamValue(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TmSQLParam(Params[I]).Value := Value[I];
    finally
      Params.Free;
    end;
  end else
    ParamByName(ParamName).Value := Value;
end;

procedure TmSQLParams.GetParamList(List: TList; const ParamNames: string);
  function ExtractFieldName(const Fields: string; var Pos: Integer): string;
  var
    I: Integer;
  begin
    I := Pos;
    while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
    Result := Trim(Copy(Fields, Pos, I - Pos));
    if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
    Pos := I;
  end;
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(ParamNames) do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;


{ TmSQLParam }

constructor TmSQLParam.Create(AParamList: TmSQLParams; AParamType: TmParamType);
begin
  if AParamList <> nil then AParamList.AddParam(Self);
  ParamType := AParamType;
  SQLDataType := SQL_UNKNOWN_TYPE;
  FBound := False;
end;

destructor TmSQLParam.Destroy;
begin
  if FParamList <> nil then FParamList.RemoveParam(Self);
end;

function TmSQLParam.IsEqual(Value: TmSQLParam): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (FData = Value.FData) and (Name = Value.Name) and
    (SQLDataType = Value.SQLDataType) and (IsNull = Value.IsNull) and
    (Bound = Value.Bound) and (ParamType = Value.ParamType);
end;

procedure TmSQLParam.SetDataType(Value: SQLSMALLINT);
begin
  FData := 0;
  FDataType := Value;
end;

function TmSQLParam.GetDataSize: Integer;
begin
  case SQLDataType of
    SQL_CHAR,
    SQL_VARCHAR,
    SQL_LONGVARCHAR: Result := Length(FData) + 1;
    SQL_DATETIME,
    SQL_DOUBLE,
    SQL_FLOAT: Result := SizeOf(Double);
    SQL_TYPE_DATE,
    SQL_TYPE_TIME,
    SQL_INTEGER: Result := SizeOf(Integer);
    SQL_SMALLINT: Result := SizeOf(SmallInt);
    SQL_BIT:      Result:= SizeOf(SQLCHAR);
    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY: Result := Length(FData);
  else
    if SQLDataType = SQL_UNKNOWN_TYPE then
      raise ESQLerror.CreateFmt( SmFieldUndefinedType, [Name]);
    raise ESQLerror.CreateFmt( SmFieldUnsupportedType, [Name]);
    Result := 0;
  end;
end;

function TmSQLParam.RecBufDataSize: Integer;
begin
  if (((SQLDataType = SQL_CHAR)
       or(SQLDataType = SQL_VARCHAR)
       or(SQLDataType = SQL_LONGVARCHAR)
      )
      and (Length(FData) > 255)
     ) or
     ((SQLDataType = SQL_BINARY)
      or(SQLDataType = SQL_VARBINARY)
      or(SQLDataType = SQL_LONGVARBINARY)) then
    Result := SizeOf(BlobParamDesc) else
    Result := GetDataSize;
end;

procedure TmSQLParam.GetData(Buffer: Pointer);
begin
  case SQLDataType of
    SQL_UNKNOWN_TYPE: raise ESQLerror.CreateFmt( SmFieldUndefinedType, [Name]);
    SQL_CHAR,
    SQL_VARCHAR,
    SQL_LONGVARCHAR: StrMove(Buffer, PChar(string(FData)), Length(FData) + 1);
    SQL_SMALLINT:    SmallInt(Buffer^) := FData;
    SQL_INTEGER:     Integer(Buffer^) := FData;
    SQL_BIT:         WordBool(Buffer^):= FData;
//    SQL_TYPE_DATE: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Date;
//    SQL_TYPE_TIME: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Time;
//    SQL_TYPE_TIMESTAMP: Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    SQL_FLOAT,
    SQL_DOUBLE: Double(Buffer^) := FData;
    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY: Move(PChar(string(FData))^, Buffer^, Length(FData));
  else
    raise ESQLerror.CreateFmt( SmFieldUnsupportedType, [Name]);
  end;
end;

procedure TmSQLParam.SetBlobData(Buffer: Pointer; Size: Integer);
var
  DataStr: string;
begin
  SetLength(DataStr, Size);
  Move(Buffer^, PChar(DataStr)^, Size);
  AsBlob := DataStr;
end;

procedure TmSQLParam.SetData(Buffer: Pointer);
var
  TimeStamp: TTimeStamp;
begin
  case SQLDataType of
    SQL_UNKNOWN_TYPE: raise ESQLerror.CreateFmt( SmFieldUndefinedType, [Name]);
    SQL_CHAR,
    SQL_VARCHAR,
    SQL_LONGVARCHAR: AsString := StrPas(Buffer);
    SQL_SMALLINT: AsSmallInt := Smallint(Buffer^);
    SQL_INTEGER: AsInteger := Integer(Buffer^);
    SQL_BIT:     AsBoolean := WordBool(Buffer^);
    SQL_TYPE_TIME:
      begin
        TimeStamp.Time := LongInt(Buffer^);
        TimeStamp.Date := DateDelta;
        AsTime := TimeStampToDateTime(TimeStamp);
      end;
    SQL_TYPE_DATE:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDate := TimeStampToDateTime(TimeStamp);
      end;
    SQL_TYPE_TIMESTAMP:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
      end;
    SQL_FLOAT,
    SQL_DOUBLE: AsFloat := Double(Buffer^);
  else
    raise ESQLerror.CreateFmt( SmFieldUnsupportedType, [Name]);
  end;
end;

procedure TmSQLParam.SetText(const Value: string);
begin
  InitValue;
  if SQLDataType = SQL_UNKNOWN_TYPE then SQLDataType := SQL_CHAR;
  FData := Value;
  case SQLDataType of
//    ftDateTime, ftTime, ftDate: FData := VarToDateTime(FData);
//    ftBCD: FData := Currency(FData);
    SQL_FLOAT,SQL_DOUBLE: FData := Single(FData);
    SQL_INTEGER,
    SQL_SMALLINT: FData := Integer(FData);
    SQL_BIT:      FData := Boolean(FData);
  end;
end;

procedure TmSQLParam.Assign(Source: TPersistent);
{
  procedure LoadFromBitmap(Bitmap: TBitmap);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      Bitmap.SaveToStream(MS);
      LoadFromStream(MS, ftGraphic);
    finally
      MS.Free;
    end;
  end;
}
  procedure LoadFromStrings(Source: TSTrings);
  begin
    AsMemo := Source.Text;
  end;

begin
  if Source is TmSQLParam then
    AssignParam(TmSQLParam(Source))
  else {if Source is TField then
    AssignField(TField(Source))
  else} if Source is TStrings then
    LoadFromStrings(TStrings(Source))
  else {if Source is TBitmap then
    LoadFromBitmap(TBitmap(Source))
  else if (Source is TPicture) and (TPicture(Source).Graphic is TBitmap) then
    LoadFromBitmap(TBitmap(TPicture(Source).Graphic))
  else }
    inherited Assign(Source);
end;

procedure TmSQLParam.AssignParam(Param: TmSQLParam);
begin
  if Param <> nil then
  begin
    SQLDataType := Param.SQLDataType;
    if Param.IsNull then Clear
    else begin
      InitValue;
      FData := Param.FData;
    end;
    FBound := Param.Bound;
    Name := Param.Name;
    if ParamType = mptUnknown then ParamType := Param.ParamType;
  end;
end;

procedure TmSQLParam.Clear;
begin
  FNull := True;
  FData := 0;
end;

procedure TmSQLParam.InitValue;
begin
  FBound := True;
  FNull := False;
end;

procedure TmSQLParam.SetAsBoolean(Value: Boolean);
begin
  InitValue;
  SQLDataType := SQL_BIT;
  FData := boolean(Value);
end;

function TmSQLParam.GetAsBoolean: Boolean;
begin
  Result := FData;
end;

procedure TmSQLParam.SetAsFloat(Value: Double);
begin
  InitValue;
  SQLDataType := SQL_DOUBLE;
  FData := Value;
end;

function TmSQLParam.GetAsFloat: Double;
begin
  Result := FData;
end;

procedure TmSQLParam.SetAsCurrency(Value: Double);
begin
  SetAsFloat(Value);
//  FDataType := ftCurrency;
end;

procedure TmSQLParam.SetAsInteger(Value: Longint);
begin
  InitValue;
  SQLDataType := SQL_INTEGER;
  FData := Value;
end;

function TmSQLParam.GetAsInteger: Longint;
begin
  Result := FData;
end;

procedure TmSQLParam.SetAsSmallInt(Value: LongInt);
begin
  SetAsInteger(Value);
  FDataType := SQL_SMALLINT;
end;

procedure TmSQLParam.SetAsString(const Value: string);
begin
  InitValue;
  SQLDataType := SQL_CHAR;
  FData := Value;
end;

function TmSQLParam.GetAsString: string;
begin
  if not IsNull then
    case SQLDataType of
      SQL_BIT:
        if FData then Result := SmTextTrue
        else Result := SmTextFalse;
      SQL_TYPE_DATE,
      SQL_TYPE_TIME,
      SQL_TYPE_TIMESTAMP: Result := VarFromDateTime(FData)
      else Result := FData;
    end
  else Result := ''
end;

procedure TmSQLParam.SetAsDate(Value: TDateTime);
begin
  InitValue;
  SQLDataType := SQL_TYPE_DATE;
  FData := VarFromDateTime(Value);
end;

procedure TmSQLParam.SetAsTime(Value: TDateTime);
begin
  SetAsDate(Value);
  FDataType := SQL_TYPE_TIME;
end;

procedure TmSQLParam.SetAsDateTime(Value: TDateTime);
begin
  SetAsDate(Value);
  FDataType := SQL_TYPE_TIMESTAMP;
end;

function TmSQLParam.GetAsDateTime: TDateTime;
begin
  if IsNull then
    Result := 0 else
    Result := VarToDateTime(FData);
end;

procedure TmSQLParam.SetAsVariant(Value: Variant);
begin
  InitValue;
  case VarType(Value) of
    varSmallint: SqlDataType := SQL_SmallInt;
    varInteger:  SqlDataType := SQL_Integer;
    varCurrency,
    varSingle,
    varDouble:   SqlDataType := SQL_DOUBLE;
    varDate:     SqlDataType := SQL_TYPE_TIMESTAMP;
    varBoolean:  SqlDataType := SQL_BIT;
    varString,
    varOleStr:   SqlDataType := SQL_CHAR;
    else         SqlDataType := SQL_UNKNOWN_TYPE;
  end;
  FData := Value;
end;

function TmSQLParam.GetAsVariant: Variant;
begin
  Result := FData;
end;

procedure TmSQLParam.SetAsMemo(const Value: string);
begin
  InitValue;
  SQLDataType := SQL_CHAR;
  FData := Value;
end;

function TmSQLParam.GetAsMemo: string;
begin
  Result := FData;
end;

procedure TmSQLParam.SetAsBlob(Value: TmBlobData);
begin
  InitValue;
  SQLDataType := SQL_BINARY;
  FData := Value;
end;

(*
procedure TmSQLParam.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TmSQLParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
var
  DataStr: string;
  Len: Integer;
begin
  with Stream do
  begin
    InitValue;
    FDataType := BlobType;
    Position := 0;
    Len := Size;
    SetLength(DataStr, Len);
    ReadBuffer(Pointer(DataStr)^, Len);
    FData := DataStr;
  end;
end;
*)

end.
