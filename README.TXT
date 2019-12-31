Version:	1.09a last updated 09 Sep 2003
Platform:	D3, D4, D5, D6, D7, CB3 & ODBC 3.0
Sources:	available
Autor:		Konstantin Savelyev
E-mail:		ksav@mail.ru
WWW:		http://go.to/modbc
Description:	ODBC data aware components


Disclaimer
----------
WARNING! THE CODE IS PROVIDED AS IS WITH NO GUARANTEES OF ANY KIND!
USE THIS AT YOUR OWN RISK - YOU ARE THE ONLY PERSON RESPONSIBLE FOR
ANY DAMAGE THIS CODE MAY CAUSE - YOU HAVE BEEN WARNED!

		
License
-------
This components are freeware (open source).


Copyright
---------
Author: Savelyev Konstantin (ksav@mail.ru), Russia.


Support 
-------

- ODBC 3.0
- editing query via
     a) internal update,insert,delete (not every drivers support it) 
     b) use SQL statements ( like TUpdateSQL )
- Blob fields and parameters
- ForwardOnly cursors without ODBC cursor library
- compact an mdb file (see access.txt)

NOT supported:
- Transactions  (you can use SQL statements)
- cached updates

INSTALLATION
------------

For Delphi 3
1) rename modbc_d3.dpk to modbc.dpk
2) check that ODBC 3.0 was installed on your computer
3) Open mODBC.dpk
4) Open each dfm form in project. Ignore following error 
   ".OldCreateOrder: Property does not exist" and save file.
5) press Compile button
6) press 'Install' button
that's all

For Delphi 4 & 5
1) rename modbc_d4.dpk(modbc_d5.dpk) to modbc.dpk
2) check that ODBC 3.0 was installed on your computer
3) Open mODBC.dpk
4) press Compile button
5) press 'Install' button
that's all

For C++Builder 3.01
1) check that ODBC 3.0 was installed on your computer
2) choose File/Open Project
3) find mODBC.bpk and press Ok button
4) choose Project/Make
5) choose Component/Install Packages
6) press 'Add' button
7) locate mODBC.bpl and press 'OK'
8) press 'ok' in 'Project Options' dialog
that's all



TESTING
-------
This components have been tested with Oracle, Informix, MySQL, 
Interbase, MS Access, PostgreSql drivers, Progress.


Notes
-----
1. For users, who use MySQL server:
   only first 3 simvols of String field are displayed. This bug of ODBC driver.
   If you set CursorType=ctDynamicCursor, then error has not occured. 
   But it is necessary to make it before each opening of the cursor.
   Also use Odbc Cursor Library( set TmDatabase.OdbcCursors:=ocUse_ODBC ).

2. If you want to use custom 'Login Dialog', you need set 
   TmDataBase.DriverCompletion=sdNoPrompt and use TmDataBase.OnConnect
   event, where you need set TmDataBase.Params property in the follow format:
   UID=<user name>
   PWD=<user password>

3. Always set TmDatabase.OdbcCursors:=ocUse_DRIVER

4. See FAQ at http://perio.unlp.edu.at/modbc


History
-------

.......
06/10/1999
- D5 compatibility
28/10/1999
- again NULL parameters in Query. I make it as BDE. If you want 
  to see version from Per-Eric Larsson, then uncomment blocks 
  marked as v1.07f in mQuery.pas
04/11/1999
- not using SQL_C_DEFAULT in SQLBindParameter
- add Driver Dialog
- support ForwardOnly cursors.
  in future I want to remove using ODBC cursor library, because
  it not correctly support blob fields.
  Now if you set CursorType=ctFwdOnlyCursor and UniDirectional=False
  mODBC create internal buffer for storing all records of dataset.
01/12/1999
- fix Calculated fields error
- move DBDriver dialog into simple combobox
03/12/1999
- new examples:
   biolife  - graphics fields
   customer - master/detail queries
17/01/2000
- several fixes from "Juan_Luis_Almar¡" <silex@arnet.com.ar>
   ( never more used StrLen()-1 statement )
- compatibility with Interbase without ODBC cursor library
25/01/2000
- deleted records with MS Access driver hided in version 4.0*
11/02/2000
- fix TmQuery.Locate and TmQuery.Lookup metods. Now it use new 
  GetFieldValue metod.
29/02/2000
- fix for Delphi3 users
- fix with MultiSelect and CompareBookmarks 
  ( thanks to "Alessandro Pilotti" <pix@skema.net>  (Skema S.r.l.))
03/03/2000
- fix time part of datetime parameters
06/03/2000
- fix if driver change attributes after open query
13/04/2000
- support wide strings 
24/04/2000
- fix locate metod for string fields
25/04/2000
- fix locate metod for partialkeys & string fields
28/04/2000
- now VARCHAR fields with length > 254 or length = 0 is ftMemo
05/05/2000
- fix PostgresSQL & SQL_VARCHAR localization do not work with fields > 255 chars
  now all char fields read as SQL_CHAR (not SQL_BINARY)
01/06/2000
- if DriverCompletion=sdNoPrompt, then ConfigureDSN do not show ODBC add dialog
03/08/2000
- small changes in function MatchVariants ( Locate metod ).
09/09/2003
- support AutoIncrement fields (tested on Access databases)
- in mTable component use InsertSQL,UpdateSQL,DeleteSQL properties only
  if driver not support bulk updates.
