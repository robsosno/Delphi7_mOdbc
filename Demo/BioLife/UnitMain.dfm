�
 TFORM1 0  TPF0TForm1Form1Left� TopyWidth�Height�CaptionAnimalsColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TPanelPanel1LeftTopWidth� Height� Hint#Scroll grid below to see other fishCaptionPanel1ParentShowHintShowHint	TabOrder  TDBTextDBLabel1LeftTop� Width� Height	DataFieldNAME
DataSourceDataSource1Font.CharsetDEFAULT_CHARSET
Font.ColorclRedFont.Height�	Font.NameMS Serif
Font.StylefsBoldfsItalic 
ParentFont  TDBImageDBImage1LeftTopWidth� Height� Hint#Scroll grid below to see other fish	DataFieldBMP
DataSourceDataSource1TabOrder    TDBGridDBGrid1LeftTop� Width�Height[Hint!Scroll up/down to see other fish!
DataSourceDataSource1Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclBlackTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style ColumnsExpanded	FieldNameNAMEVisible	 Expanded	FieldNameSIZ_Visible	 Expanded	FieldNameWEIGHTVisible	 Expanded	FieldNameAREAVisible	    TPanelPanel2Left� Top
WidthHeight� CaptionPanel2TabOrder TDBMemoDBMemo1LeftTopWidth� Height� 	DataFieldComment
DataSourceDataSource1TabOrder    TButtonButton3Left�Top� Width=HeightCaptionreopenTabOrderOnClickButton3Click  TButtonButton4LeftTop� Width[HeightCaptionCopyTabOrderOnClickButton4Click  TButtonButton5LeftkTop� Width^HeightCaptionPasteTabOrderOnClickButton5Click  TDBNavigatorDBNavigator1Left� Top� Width� Height
DataSourceDataSource1TabOrder  TButtonButton1Left�Top_Width=HeightCaptionaddTabOrderOnClickButton1Click  TDataSourceDataSource1DataSetmTable1LeftTop�   
TmDataBase
mDataBase1Params.StringsDBQ=..\DB\demo.mdb&DRIVER=Microsoft Access Driver (*.mdb) 
WaitCursor	crSQLWaitShowWaitCursorLeft� Top*  TmTablemTable1DataBase
mDataBase1RequestLive		TableNameanimalsIndexFieldNamesnameLeftoTop�   
TmDataBase
mDataBase2DataBaseNameemployeeParams.StringsDSN=employee 
WaitCursor	crSQLWaitShowWaitCursorLeftoTop�   TmQuerymQuery1SQL.StringsSELECT   * FROM ANIMALS ModifySQL.Stringsupdate ANIMALSset  SIZ_ = :SIZ_,AREA = :AREAWhere NAME = :OLD_NAME InsertSQL.Stringsinsert into ANIMALS(NAME,SIZ_,WEIGHT,AREA,BMP,COMMENT)values (:NAME,:SIZ_,:WEIGHT,:AREA,:BMP,:COMMENT) DeleteSQL.Stringsdelete from ANIMALSwhereNAME = :OLD_NAME DataBase
mDataBase2RequestLive	Left� Top�    