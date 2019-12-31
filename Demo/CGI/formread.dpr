program formread;

uses
  CGIClass,mDataBas,mQuery;
Var
   m: TmQuery;
   d: TmDataBase;
   i:integer;
begin
  with TStandardCGI.Create do
  try
    OutgoingContentType := 'text/html';

    d:=TmDataBase.Create(nil);
    m:=TmQuery.Create(nil);

    d.DataBaseName:=FormVariable['datasource'];
    d.DriverCompletion:=sdNoPrompt;
    d.Params.add('UID='+FormVariable['UID']
                +';PWD='+FormVariable['PWD']);
    m.DataBase:=d;

    m.SQL.Add('select * from '+FormVariable['tablename'] );
    m.Open;
    WriteHeaders;
    Writeln('<HTML><BODY>');
    writeln('<CENTER>Table :'+FormVariable['tablename']+'</CENTER>');
    writeln('<TABLE BORDER=1>');
    writeln('<TR>');
    for i:=0 to m.fieldcount-1 do
         begin
            writeln('<TD><CENTER>',m.fields[i].FieldName,'</CENTER></TD>');
         end;
    writeln('</TR>');

    while not m.eof do
     begin
        writeln('<TR>');
        for i:=0 to m.fieldcount-1 do
         begin
            writeln('<TD>',m.fields[i].asstring,'</TD>');
         end;
        writeln('</TR>');
        m.next;
     end;
    writeln('</TABLE>');
    m.close;
    d.disconnect;
    Writeln('</BODY></HTML>');
  finally
    m.free;
    d.free;
    Free;
  end;
end.

