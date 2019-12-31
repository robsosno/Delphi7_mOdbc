unit mTable;
{$I mODBC.INC}

interface

uses Classes, SysUtils, mQuery;

type
  TmTable = class(TmCustomQuery)
  private
    { Private declarations }
    FTableName: String;
    FIndexFieldNames: String;
    FMasterFieldNames: String;
//    procedure SetIndexFieldNames( fName:String);
  protected
    procedure MakeSQL( tName, iStr, mStr:String;var iSQL,uSQL,dSQL: TStrings); // generate SQL statements
    procedure DoOnNewRecord; override;
    procedure InternalOpen; override;
  public
    { Public declarations }
    procedure SetTableName( tName:String);
    procedure GetFieldNames( ATableName : string; AList : TStrings);
    procedure GetUniqueFieldNames( ATableName : string; AList : TStrings );
//    procedure PrepareCursor; override;
  published
    { Published declarations }
    property Active;
    property AutoCalcFields;
    property DataBase;
    property DataSource;
    property RequestLive;
    property CursorType;

    property TableName:String read FTableName write SetTableName;
    property IndexFieldNames:String read FIndexFieldNames write FIndexFieldNames;
    property MasterFieldNames:String read FMasterFieldNames write FMasterFieldNames;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation
uses db,odbcsql;

procedure TmTable.SetTableName( tName:String);
//var
//   Activated:boolean;
begin
  if FTableName <> tName then
  begin
//          activated:=Active;
    if Active then
      Close;
    UnPrepareCursor;
    FTableName := tName;
    InsertSQL.Text:='';
//          if activated then Open;
  end;
end;

{
procedure TmTable.SetIndexFieldNames( fName:String);
begin
    if (csReading in ComponentState)
       or (fName='') then
    begin
        FIndexFieldNames := fName;
        exit;
    end;
    MakeSQL( FTableName, fName);
end;
}

  function extractfield(s:string; n:integer):String;
  var
    i,j: integer;
  begin
    i := 1;
    repeat
      j := i;

      while i <= length(s) do
      begin
        if (s[i] in [' ',',',';']) then
          break;
        inc( i);
      end;

      if n = 1 then
      begin
        result := Copy( s, j, i-1);
        exit;
      end;

      while (i <= length(s)) and (s[i] in [' ',',',';']) do
        inc(i);

     dec(n);
    until i > length(s);

    result := '';
  end;

procedure TmTable.MakeSQL( tName, iStr, mStr:String; var iSQL,uSQL,dSQL: TStrings);
var
  fNames,
  uNames: TStringList;
  i:      integer;
  AList:  TStringList;
  Str,
  condition,
  ifield,
  mfield:   String;
begin
  if not Assigned( DataBase) then
      raise Exception.Create('DataBase property not assigned');
  DataBase.Connect;

  fNames:=TStringList.Create;
  uNames:=TStringList.Create;
  AList :=TStringList.Create;
  try
     GetFieldNames( tName, fNames);
     GetUniqueFieldNames( tName, uNames);
     if (uNames.Count=0) and RequestLive
        then raise Exception.Create('Table '+tname+' does not consist key fields');
     AList.Clear;
//     AList.Add('SELECT');
     for i:=0 to uNames.Count-1 do
       if fNames.IndexOf( uNames[i])<0
         then if AList.Count>0 then AList.add('   ,'+uNames[i])
                               else AList.add('SELECT '+uNames[i]);
     if AList.Count>0 then AList.add('   ,*')
                      else AList.add('SELECT *');
     AList.add('  FROM '+tname);

     // add condition
     if Assigned( DataSource) and (length(mStr)>0) then
     begin
        i := 1;
        condition := '';
        repeat
           ifield := extractField( iStr, i);
           mfield := extractField( mStr, i);

           if mfield = '' then
             break;

           if ifield = '' then
             raise exception.create('index field not defined');

           if length(condition) > 0 then
             condition := condition + ' and ';

           condition := condition + '(' + ifield + '=:' + mfield +')';
           inc( i);
        until false;
        AList.add('  WHERE '+condition);
     end;

     if length(iStr) > 0 then
     begin
       Str := iStr;
       while pos(';', Str) > 0 do
         Str[pos(';', Str)] := ',';
       AList.add('  ORDER BY '+Str);
     end;
     SQL.assign( AList);

     AList.Clear;
     if RequestLive then
     begin
       AList.Add('INSERT INTO '+tname);
       for i:=0 to fNames.Count-1 do
         if i>0 then AList.add('    ,'+fNames[i])
                else AList.add('    ('+fNames[i]);
       AList.add('    ) VALUES');
       for i:=0 to fNames.Count-1 do
         if i>0 then AList.add('    ,:'+fNames[i])
                else AList.add('    (:'+fNames[i]);
       AList.add('    )');
     end;
     iSQL.assign( AList);

     AList.Clear;
     if RequestLive then
     begin
       AList.Add('UPDATE '+tname+' SET');
       for i:=0 to fNames.Count-1 do
         if i>0 then AList.add('    ,'+fNames[i]+'=:'+fNames[i])
                else AList.add('    '+fNames[i]+'=:'+fNames[i]);
       AList.add('  WHERE');
       for i:=0 to uNames.Count-1 do
         if i>0 then AList.add('    AND '+uNames[i]+'=:old_'+uNames[i])
                else AList.add('    '+uNames[i]+'=:old_'+uNames[i]);
     end;
     uSQL.assign( AList);

     AList.Clear;
     if RequestLive then
     begin
       AList.Add('DELETE FROM '+tname+' WHERE');
       for i:=0 to uNames.Count-1 do
         if i>0 then AList.add('    AND '+uNames[i]+'=:old_'+uNames[i])
                else AList.add('    '+uNames[i]+'=:old_'+uNames[i]);
       end;
     dSQL.assign( AList);
//       FTableName := tName;
//       FIndexFieldNames := fName;
  finally
     fNames.free;
     uNames.free;
     AList.free;
  end;
end;
{
procedure TmTable.SetQuery(Value: TStrings);
begin
    Inherited SetQuery( Value);
    if not (csReading in ComponentState) then FTableName:='';
end;

procedure TmTable.SetUpdateSQL(Index: Integer; Value: TStrings);
begin
    Inherited SetUpdateSQL(Index, Value);
    if not (csReading in ComponentState) then FTableName:='';
end;
}

procedure TmTable.InternalOpen;
var
   iSQL,uSQL,dSQL: TStrings;
   needMakeSQL: boolean;
begin
  iSQL := TStringList.Create;
  uSQL := TStringList.Create;
  dSQL := TStringList.Create;
  try
     needMakeSQL := InsertSQL.Text='';
     if needMakeSQL then
       MakeSQL(FTableName, FIndexFieldNames, FMasterFieldNames,iSQL,uSQL,dSQL);
     inherited InternalOpen;
     if needMakeSQL and (CursorAttr.UseInternalInsert=False) then
       InsertSQL.assign( iSQL);
     if needMakeSQL and (CursorAttr.UseInternalUpdate=False) then
       ModifySQL.assign( uSQL);
     if needMakeSQL and (CursorAttr.UseInternalDelete=False) then
       DeleteSQL.assign( dSQL);
  finally
     iSQL.free;
     uSQL.free;
     dSQL.free;
  end;
end;

procedure TmTable.GetFieldNames( ATableName : string; AList : TStrings );
var
  h : SQLHANDLE;
  AFieldName : array[0..SQL_NAME_LEN + 1] of char;
	DSName : array[0..SQL_NAME_LEN + 1] of char;
  l : LongInt;
  Res : SQLRETURN;
begin
  AList.Clear;

  DataBase.Connect;
  if SQLAllocHandle( SQL_HANDLE_STMT,DataBase.hdbc,h ) = SQL_SUCCESS then
  try
     StrPCopy( DSName, ATableName );
     Res := SQLColumns( h,nil,0,nil,0,@DSName[0],SQL_NTS,nil,0 );
     if Res = SQL_SUCCESS then
     begin
        SQLBindCol( h,4,SQL_CHAR,@AFieldName[0],SQL_NAME_LEN,@l );
        //if Assigned( ATypes ) then
        //SQLBindCol( h,5,SQL_SMALLINT,@DataType,0,@cbDataType );
        Res := SQLFetch( h );
        while ( Res = SQL_SUCCESS ) do
        begin
          AList.Add( StrPas( AFieldName ) );
          //if Assigned( ATypes ) then
          //begin
          //  AField := TDataSetFieldInfo.Create;
          //  AField.DataType := DataType;
          //  ATypes.Add( AField );
          //end;
          Res := SQLFetch( h );
        end;
     end;
  finally
     SQLFreeHandle( SQL_HANDLE_STMT,h );
  end;
end;//GetFieldNames

procedure TmTable.GetUniqueFieldNames( ATableName : string; AList : TStrings );
var
  h : SQLHANDLE;
	AFieldName : array[0..SQL_NAME_LEN + 1] of char;
	DSName : array[0..SQL_NAME_LEN + 1] of char;
  l : LongInt;
  Res : SQLRETURN;
begin
  AList.Clear;

  DataBase.Connect;
  if SQLAllocHandle( SQL_HANDLE_STMT,DataBase.hdbc,h ) = SQL_SUCCESS then
  try
    StrPCopy( DSName, ATableName );
    Res := SQLSpecialColumns( h, SQL_BEST_ROWID, nil, 0, nil, 0,
                              @DSName[0], SQL_NTS, SQL_SCOPE_CURROW,
                              SQL_NULLABLE);
    if Res = SQL_SUCCESS then
    begin
      SQLBindCol( h, 2, SQL_CHAR, @AFieldName[0], SQL_NAME_LEN, @l );
      Res := SQLFetch( h );
      while ( Res = SQL_SUCCESS ) do
      begin
        AList.Add( StrPas( AFieldName ) );
        Res := SQLFetch( h );
      end;
    end;
  finally
    SQLFreeHandle( SQL_HANDLE_STMT,h );
  end;
end;

procedure TmTable.DoOnNewRecord;
var
  I: Integer;
//  DataSet: TDataSet;
  fd,fd1: TField;
  ifield,mfield:String;
begin

   if Assigned( DataSource)
      and(DataSource.DataSet<>nil)
      and(DataSource.DataSet.Active)  then
     begin
        i := 1;
        repeat
           ifield := extractField( FIndexFieldNames, i);
           mfield := extractField( FMasterFieldNames, i);

           if mfield = '' then
             break;

           if ifield = '' then
             raise exception.create('index field not defined');

           fd:=FindField(ifield);
           fd1:=DataSource.DataSet.FindField(mfield);
           if (fd <> nil)and(fd1 <> nil) then
           begin
             fd.Assign(fd1);
           end;
           inc( i);
        until false;
     end;
  inherited DoOnNewRecord;
end;

end.
