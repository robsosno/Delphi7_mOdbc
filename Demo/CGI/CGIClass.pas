unit CGIClass;
interface

uses
  Classes;

type
  TStandardCGI = class
  private
    FRemoteHost: string;
    FRemoteAddress: string;
    FContentLength: string;
    FRequestMethod: string;
    FQueryString: string;
    FMimeType: string;
    FLocation: string;
    FOutgoingContentLength: Integer;
    FOutgoingContentType: string;
    FOutgoingCookies: TStringList;
    FFormVariables: TStringList;
    FIncomingData: string;
    FCookieExpires: TDateTime;
    FCookieDomain: string;
    FCookiePath: string;
    function GetIncomingCookie(CookieName: string): string;
    function GetOutgoingCookie(CookieName: string): string;
    procedure SetOutgoingCookie(CookieName: string; Value: string);
    function GetFormVariable(VariableName: string): string;
    function GetRemoteHost: string;
    function GetRemoteAddress: string;
    function GetContentLength: Integer;
    function GetRequestMethod: string;
    function GetQueryString: string;
  protected
    //utility methods
    function GetItemIndex(VarName: string; List: TStrings): Integer;
    function CompNC(Str1, Str2: string): Boolean;
    function PosNC(SubStr, SearchString: string): Integer;
    procedure FindReplace(var Str: string; FindStr, ReplaceStr: string);
    procedure LoadFormVariables;
    function GetIncomingData: string;
    function GetEnvironmentVar(VariableName: string; Size: Integer): string;
    function HexDigit(C: Char): Byte;
    function HexToChar(Str: string): Char;
  public
    constructor Create;
    destructor Destroy; override;
    //incoming properties
    property IncomingCookie[CookieName: string]: string read GetIncomingCookie;
    property FormVariable[VariableName: string]: string read GetFormVariable;
    property RemoteHost: string read GetRemoteHost;
    property RemoteAddress: string read GetRemoteAddress;
    property ContentLength: Integer read GetContentLength;
    property RequestMethod: string read GetRequestMethod;
    property QueryString: string read GetQueryString;
    property IncomingData: string read GetIncomingData;
    //outgoing properties and methods
    property OutgoingContentLength: Integer read FOutgoingContentLength write FOutgoingContentLength;
    property OutgoingContentType: string read FOutgoingContentType write FOutgoingContentType;
    property OutgoingCookie[CookieName: string]: string read GetOutgoingCookie write SetOutgoingCookie;
    property CookieExpires: TDateTime read FCookieExpires write FCookieExpires;
    property CookieDomain: string read FCookieDomain write FCookieDomain;
    property CookiePath: string read FCookiePath write FCookiePath;
    procedure WriteHeaders;
    procedure WriteOutFormVariables;
    procedure WriteOutFormVariablesToFile(FileName: string);
    function SendFile(FileName: string; SendHeaders: Boolean): Boolean;
    property Relocation: string read FLocation write FLocation;
    //utility functions
    function EncodeHTTP(HTTPStr: string): string;
    function DecodeHTTP(HTTPStr: string): string;
    function WinExecuteWait(FileName: string; TimeOut: Integer): Boolean;
    function HRef(Location, Text: string): string;
    function Image(Location: string): string;
    procedure WriteBRs(Count: Integer);
    procedure WriteHRs(Count: Integer);
  end;

implementation

uses
  Windows,
  SysUtils;

const
  ValidHTTPChars: set of char = ['A'..'Z','a'..'z','~','_','0'..'9'];
  ContentLengthStr = 'Content-Length: ';
  ContentTypeStr = 'Content-type: ';
  SetCookieStr = 'Set-Cookie: ';
  LocationStr = 'Location: ';
  CookieDateFormatStr = 'dddd", "dd"-"mmm"-"yy" "hh":"mm":"ss" GMT"';


constructor TStandardCGI.Create;
begin
  Inherited Create;
  FOutgoingContentType := 'text/html';
  FOutgoingContentLength := 0;
  FIncomingData := '';
  FLocation := '';
  FCookieExpires := 0;
  FCookieDomain := '';
  FCookiePath := '';
end;

destructor TStandardCGI.Destroy;
begin
  Inherited Destroy;
end;

function TStandardCGI.PosNC(SubStr, SearchString: string): Integer;
begin
  Result := Pos(Uppercase(SubStr), Uppercase(SearchString));
end;

function TStandardCGI.GetIncomingCookie(CookieName: string): string;
var
  CookieData: string;
  StrPosition: Integer;
begin
  Result := '';
  CookieData := GetEnvironmentVar('HTTP_COOKIE',1024);
  StrPosition := PosNC(CookieName, CookieData);
  if StrPosition = 0 then Exit;
  Delete(CookieData, 1, StrPosition + Length(CookieName));
  StrPosition := Pos(';', CookieData);
  if StrPosition <> 0 then Delete(CookieData, StrPosition, Length(CookieData));
  Result := CookieData;
end;

function TStandardCGI.CompNC(Str1, Str2: string): Boolean;
begin
  Result := Uppercase(Str1) = Uppercase(Str2);
end;

function TStandardCGI.GetItemIndex(VarName: string; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := -1;
  VarName := VarName + '=';
  for I := 0 to List.Count - 1 do
    if CompNC(Copy(List.Strings[I], 1, Length(VarName)), VarName) then
    begin
      Result := I;
      break;
    end;
end;

function TStandardCGI.GetOutgoingCookie(CookieName: string): string;
var
  CookieIndex: Integer;
begin
  Result := '';
  if FOutgoingCookies = nil then Exit;
  CookieIndex := GetItemIndex(CookieName, FOutgoingCookies);
  if CookieIndex < 0 then Exit;
  Result := Copy(FOutgoingCookies.Strings[CookieIndex], Length(CookieName) + 1,
    Length(FOutgoingCookies.Strings[CookieIndex]));
end;

procedure TStandardCGI.SetOutgoingCookie(CookieName: string; Value: string);
var
  CookieIndex: Integer;
begin
  if FOutgoingCookies = nil then FOutgoingCookies := TStringList.Create;
  CookieIndex := GetItemIndex(CookieName, FOutgoingCookies);
  if CookieIndex < 0 then
    FOutgoingCookies.Add(CookieName + '=' + Value)
  else
    FOutgoingCookies.Strings[CookieIndex] := CookieName + '=' + Value;
end;

function TStandardCGI.GetIncomingData: string;
var
  DataBuffer: PChar;
  Overlapped: TOverlapped;
  BytesRead: Integer;
  I: Integer;
  C: Char;
begin
  if FIncomingData <> '' then
  begin
    Result := FIncomingData;
    Exit;
  end;
  Result := '';
  if RequestMethod = 'POST' then
  begin
    GetMem(DataBuffer, ContentLength + 1);
    try
      for I := 1 to ContentLength do
      begin
        Read(C);
        Databuffer[I - 1] := C;
      end;
      Databuffer[ContentLength] := #0;
      Result := StrPas(DataBuffer);
    finally
      FreeMem(DataBuffer);
    end;
  end
  else
    Result := GetQueryString;
end;

procedure TStandardCGI.LoadFormVariables;
var
  InputData: string;
  AmpPos: Integer;
begin
  InputData := GetIncomingData;
  while (Length(InputData) > 0) do
  begin
    AmpPos := Pos('&', InputData);
    if Pos('&', InputData) <> 0 then
    begin
      FFormVariables.Add(Copy(InputData, 1, AmpPos - 1));
      Delete(InputData, 1, AmpPos);
    end else
    begin
      FFormVariables.Add(InputData);
      break;
    end;
  end;
end;

function TStandardCGI.GetFormVariable(VariableName: string): string;
var
  VariableIndex: Integer;
begin
  if FFormVariables = nil then
  begin
    FFormVariables := TStringList.Create;
    LoadFormVariables;
  end;
  Result := '';
  VariableIndex := GetItemIndex(VariableName, FFormVariables);
  if VariableIndex < 0 then Exit;
  Result := Copy(FFormVariables.Strings[VariableIndex], Length(VariableName) + 2,
    Length(FFormVariables.Strings[VariableIndex]));
  Result := DecodeHTTP(Result);
end;

function TStandardCGI.GetRemoteHost: string;
begin
  if FRemoteHost = '' then
    FRemoteHost := GetEnvironmentVar('REMOTE_HOST', 128);
  Result := FRemoteHost;
end;

function TStandardCGI.GetRemoteAddress: string;
begin
  if FRemoteAddress = '' then
    FRemoteAddress := GetEnvironmentVar('REMOTE_ADDR', 128);
  Result := FRemoteAddress;
end;

function TStandardCGI.GetContentLength: Integer;
begin
  if FContentLength = '' then
    FContentLength := GetEnvironmentVar('CONTENT_LENGTH', 128);
  Result := StrToInt(FContentLength);
end;

function TStandardCGI.GetRequestMethod: string;
begin
  if FRequestMethod = '' then
    FRequestMethod := GetEnvironmentVar('REQUEST_METHOD', 128);
  Result := FRequestMethod;
end;

function TStandardCGI.GetQueryString: string;
begin
  if FQueryString = '' then
    FQueryString := GetEnvironmentVar('QUERY_STRING', 1024);
  Result := FQueryString;
end;


function TStandardCGI.GetEnvironmentVar(VariableName: string; Size: Integer): string;
var
  PEnvironmentVar: PChar;
begin
  GetMem(PEnvironmentVar, Size);
  try
    GetEnvironmentVariable(PChar(VariableName), PEnvironmentVar, Size);
    Result := StrPas(PEnvironmentVar);
  finally
    FreeMem(PEnvironmentVar);
  end;
end;

procedure TStandardCGI.FindReplace(var Str: string; FindStr, ReplaceStr: string);
var
  Position: Integer;
begin
  if Pos(FindStr, ReplaceStr) <> 0 then
  begin
    if Pos(FindStr, #25) <> 0 then Exit;  {commenting this line assumes no #25...runs a little faster maybe}
    FindReplace(Str, FindStr, #25);
    FindReplace(Str, #25, ReplaceStr);
  end else
  begin
    Position := Pos(FindStr, Str);
    while Position <> 0 do
    begin
      Delete(Str, Position, Length(FindStr));
      insert(ReplaceStr, Str, Position);
      Position := Pos(FindStr, Str);
    end;
  end;
end;

function TStandardCGI.HexDigit(C: Char): Byte;
begin
  Result := 0;
  C := Upcase(C);
  if (C >= '0') and (C <= '9') then Result := Ord(C) - Ord('0');
  if (C >= 'A') and (C <= 'F') then Result := Ord(C) - Ord('A') + 10;
end;

function TStandardCGI.HexToChar(Str: string): Char;
begin
  if Length(Str) < 2 then Exit;
  Result := Chr(16 * HexDigit(Str[1]) + HexDigit(Str[2]));
end;

function TStandardCGI.EncodeHTTP(HTTPStr: string): string;
var
  I: Integer;
  C: Char;
begin
  I := 1;
  FindReplace(HTTPStr, '!', '!21');
  while I <= Length(HTTPStr) do
  begin
    if (not (HTTPStr[I] in ValidHTTPChars)) and (HTTPStr[I] <> ' ') and
      (HTTPStr[I] <> '!') then
    begin
      FindReplace(HTTPStr, HTTPStr[I], '!' + IntToHex(Ord(HTTPStr[I]), 2));
      I := I + 2;
    end
    else
      I := I + 1;
    FindReplace(HTTPStr, ' ', '+');
    Result := HTTPStr;
  end;
end;


function TStandardCGI.DecodeHTTP(HTTPStr: string): string;
  function GetFirst: Char;
  begin
    if Length(HTTPStr) > 0 then Result := HTTPStr[1] else Result := #0;
  end;
begin
  Result := '';
  FindReplace(HTTPStr, '+', ' ');
  FindReplace(HTTPStr, '%', '!');
  while Length(HTTPStr) > 0 do
  begin
    if GetFirst = '!' then
    begin
      Delete(HTTPStr, 1, 1);
      if GetFirst = '!' then
      begin
        Result := Result + '!';
        Delete(HTTPStr, 1, 1);
      end else
      begin
        Result := Result + HexToChar(Copy(HTTPStr, 1, 2));
        Delete(HTTPStr, 1, 2);
      end;
    end else
    begin
      Result := Result + GetFirst;
      Delete(HTTPStr, 1, 1);
    end;
  end;
end;

procedure TStandardCGI.WriteHeaders;
var
  I: Integer;
  function FormatCookie(Cookie: string): string;
  begin
    if FCookiePath <> '' then
      Cookie := Cookie + '; path=' + FCookiePath;
    if FCookieDomain <> '' then
      Cookie := Cookie + '; domain=' + FCookieDomain;
    if FCookieExpires <> 0 then
      Cookie := Cookie + '; expires=' +
        FormatDateTime(CookieDateFormatStr, FCookieExpires);
    Result := Cookie;
  end;
begin
  if FLocation = '' then
  begin
    if FOutgoingContentLength <> 0 then
      Writeln(ContentLengthStr + IntToStr(FOutgoingContentLength));
    if FOutgoingContentType <> '' then
      Writeln(ContentTypeStr + FOutgoingContentType);
  end
  else
    Writeln(LocationStr + FLocation);
  try
    if FOutgoingCookies <> nil then
      for I := 0 to FOutgoingCookies.Count - 1 do
        Writeln(SetCookieStr + FormatCookie(FOutgoingCookies.Strings[I]));
  except
    {}
  end;
  Writeln('');
end;

function TStandardCGI.SendFile(FileName: string; SendHeaders: Boolean): Boolean;
var
  IncomingFile: file of byte;
  FileBuffer: PChar;
  I: Integer;
begin
  Result := False;
  AssignFile(IncomingFile, FileName);
  Reset(IncomingFile);
  try
    OutgoingContentLength := FileSize(IncomingFile);
    if SendHeaders then WriteHeaders;
    GetMem(FileBuffer, OutgoingContentLength + 1);
    try
      BlockRead(IncomingFile, FileBuffer^, OutgoingContentLength);
      for I := 0 to OutgoingContentLength - 1 do Write(Output, FileBuffer[I]);
      Result := True;
    finally
      FreeMem(FileBuffer);
    end;
  finally
    CloseFile(IncomingFile);
  end;
end;


procedure TStandardCGI.WriteOutFormVariables;
var
  I: Integer;
begin
  if FFormVariables = nil then
  begin
    FFormVariables := TStringList.Create;
    LoadFormVariables;
  end;
  for I := 0 to FFormVariables.Count - 1 do
    Writeln(DecodeHTTP(FFormVariables.Strings[I]) + '<BR>');
end;

procedure TStandardCGI.WriteOutFormVariablesToFile(FileName: string);
var
  I,J: Integer;
  FileOut: TextFile;
begin
  if FileName = '' then Exit;
  AssignFile(FileOut, FileName);
  if FileExists(FileName) then Append(FileOut) else Rewrite(FileOut);
  try
    Writeln(FileOut, '<HR>(' + TimeToStr(Time) + ', ' + DateToStr(Date) +
      ', ' + RemoteHost + ')<BR>');
    for I := 0 to FFormVariables.Count - 1 do
      Writeln(FileOut, DecodeHTTP(FFormVariables.Strings[I]) + '<BR>');
  finally
    CloseFile(FileOut);
  end;
end;

function TStandardCGI.WinExecuteWait(FileName: string; TimeOut: Integer): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if TimeOut = 0 then TimeOut := INFINITE;
  if (CreateProcess(nil, pchar(FileName), Nil, Nil, FALSE, 0, nil, nil,
    StartupInfo, Processinfo)) then
    Result := WaitForSingleObject(ProcessInfo.Hprocess, TimeOut) = WAIT_OBJECT_0
  else
    Result := False;
end;

function TStandardCGI.HRef(Location, Text: string): string;
begin
  Result := '<A HREF="' + Location + '">' + Text + '</A>';
end;

function TStandardCGI.Image(Location: string): string;
begin
  Result := '<IMG SRC="' + Location + '">';
end;

procedure TStandardCGI.WriteBRs(Count: Integer);
var
  I: Integer;
begin
  for I := 1 to Count do
    Writeln('<BR>');
end;

procedure TStandardCGI.WriteHRs(Count: Integer);
var
  I: Integer;
begin
  for I := 1 to Count do
    Writeln('<HR>');
end;


end.

