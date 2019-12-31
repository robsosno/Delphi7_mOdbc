{#B+ scanhelp directive}
{#G+ scanhelp directive}
{#F+ scanhelp directive}

{***************************************************************
 *
 * Unit Name: msAccessSupportUnit
 * Purpose  : Support unit for mODBC to make it possible to
              create , repair and compact msaccess databases
 * Author   : Per-Eric Larsson mailto:per-eric.larsson@home.se
 * History  : -
 *
 ****************************************************************}

unit msAccessSupportUnit;

interface
uses
  forms,
  sysutils,
  odbcsql;
type
  AccessVersion = (Access2, Access95, Access97, Access2000);
  AccessAction = (CREATE_DB, REPAIR_DB, COMPACT_DB);

function AccessAccess(const Action: AccessAction; const Version: AccessVersion;
  const dbname, compactdbname: string): integer;
implementation

function AccessAccess(const Action: AccessAction; const Version: AccessVersion;
  const dbname, compactdbname: string): integer;
var
  s: string;
const
  szDriver = 'Microsoft Access Driver (*.mdb)';
begin
  case Action of
    CREATE_DB: begin
        case Version of
          Access2: begin
              s := format('CREATE_DBV2="%s" General', [dbname]);
            end;
          Access95,
            Access97: begin
              s := format('CREATE_DBV3="%s" General', [dbname]);
            end;
          Access2000: begin
              s := format('CREATE_DBV4="%s" General', [dbname]);
            end;
        end;
        result := SQLConfigDataSource(Application.Handle, ODBC_ADD_DSN, szDriver, s);
      end;
    REPAIR_DB: begin
        s := format('REPAIR_DB="%s" General', [dbname]);
        result := SQLConfigDataSource(Application.Handle, ODBC_ADD_DSN, szDriver, s);
      end;
    COMPACT_DB: begin
        if compactdbname = '' then
        begin
          s := format('COMPACT_DB="%s" "%s" General', [dbname, dbname]);
        end
        else
        begin
          s := format('COMPACT_DB="%s" "%s" General', [dbname, compactdbname]);
        end;
        result := SQLConfigDataSource(Application.Handle, ODBC_ADD_DSN, szDriver, s);
      end;
    else
      result:=-1;
  end;
end;
end.

