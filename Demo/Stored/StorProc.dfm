�
 TFORM1 0�  TPF0TForm1Form1Left� ToptWidth�Height� Caption"Example of using stored proceduresColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnCreate
FormCreatePixelsPerInch`
TextHeight TDBGridDBGrid1Left Top Width|Height� AlignalClient
DataSourceDataSource1TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   TPanelPanel1Left Top� Width|Height!AlignalBottom
BevelOuterbvNoneTabOrder TButtonButton1Left
TopWidthiHeightCaption	Add StockTabOrder OnClickButton1Click  TEdit	StockNameLeftTopWidth� HeightTabOrder   
TStatusBar
StatusBar1Left Top� Width|HeightPanels SimplePanel  
TmDataBase
mDataBase1DataBaseNameALTA_DATA_SOURCELeft
Top  TmStoredProcmStoredProc1DataBase
mDataBase1StoredProcNameAddSampleStockIsShowError		ErrorCode Left0Top  TmQuerymQuery1SQL.StringsSELECT ID,NikNameFROM StockSample DataBase
mDataBase1
CursorTypectKeySetCursorLeftZTop  TDataSourceDataSource1DataSetmQuery1Left|Top   