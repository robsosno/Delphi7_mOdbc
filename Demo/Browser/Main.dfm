˙
 TFORM1 09  TPF0TForm1Form1LeftÂ TopkWidthˇHeightPCaptionODBC SQL query demo programColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightő	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnCreate
FormCreatePixelsPerInch`
TextHeight 	TSplitter	Splitter3LeftuTop WidthHeight5CursorcrHSplit  TPanelPanel2LeftxTop Width7Height5AlignalClient
BevelOuterbvNoneTabOrder  TPanelPanel1Left Top Width7Height)AlignalTopTabOrder  TBitBtnBtnCloseLeftô TopWidthKHeightCaption	Close SQLTabOrder OnClickBtnCloseClick  TDBNavigatorDBNavigator1LeftTopWidthć Height
DataSourceDataSource1TabOrder  TEditEdit1LeftHTopWidthyHeightTabOrderTextEdit1  TBitBtnBtnFindLeftÄTopWidth-HeightCaptionFindTabOrderOnClickBtnFindClick  TButtonButton1Left TopWidth1HeightCaptionButton1TabOrderOnClickButton1Click   	TNotebook	Notebook1Left Top)Width7HeightAlignalClient	PageIndexTabOrder TPage Left Top CaptionDefault TPageControlPageControl1Left Top(Width7Heightä 
ActivePage	TabSheet1AlignalClientTabOrder  	TTabSheet	TabSheet1Captionselect statement TMemoMemo1Left Top Width/HeightČ AlignalClientLines.Stringsselect * from accounts  TabOrder    	TTabSheet	TabSheet2CaptionInsert statement TMemoMemo2Left Top WidthHeightÓ AlignalClientTabOrder    	TTabSheet	TabSheet3Captionupdate statement TMemoMemo3Left Top WidthHeightÓ AlignalClientTabOrder    	TTabSheet	TabSheet4Captiondelete statement TMemoMemo4Left Top WidthHeightÓ AlignalClientTabOrder     TPanelPanel5Left Top Width7Height(AlignalTopTabOrder TBitBtnBtnOpenLeftTopWidthKHeightCaptionOpen SQLTabOrder OnClickBtnOpenClick  TBitBtnBtnExecLeft\TopWidthKHeightCaptionExecute SQLTabOrderOnClickBtnExecClick  TBitBtnBtnPrevLeft´ TopWidthHeightCaption<TabOrder  TBitBtnBtnNextLeftĚ TopWidthHeightCaption>TabOrder    TPage Left Top Captionresult 	TSplitter	Splitter1Left TopĹ Width7HeightCursorcrVSplitAlignalBottom  	TSplitter	Splitter2Left TopÂ Width7HeightCursorcrVSplitAlignalBottom  TDBGridDBGrid1Left Top Width7HeightÂ AlignalClient
DataSourceDataSource1TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightőTitleFont.NameMS Sans SerifTitleFont.Style 
OnColEnterDBGrid1ColEnter  TDBMemoDBMemo1Left TopČ Width7HeightDAlignalBottom
DataSourceDataSource1TabOrder     TPanelPanel3Left Top WidthuHeight5AlignalLeft
BevelOuterbvNoneBorderWidthCaptionPanel3TabOrder TListBoxListBox1LeftTop!WidthoHeightAlignalClient
ItemHeightTabOrder OnClickListBox1Click
OnDblClickListBox1DblClick  TPanelPanel4LeftTopWidthoHeightAlignalTop
BevelOuterbvNoneTabOrder TSpeedButtonSBdsnLeft Top WidthHeight
GroupIndexDown	
Glyph.Data
j  f  BMf      v   (               đ                                      ŔŔŔ   ˙  ˙   ˙˙ ˙   ˙ ˙ ˙˙  ˙˙˙                 ˙ř˙˙˙đ  DřôDOđ   ˙ř˙˙˙đ  DřôDDđ   ˙ř˙˙˙đ  DřôDOđ  ˙ř˙˙˙đ  DřôDDđ  ˙ř˙˙˙đ            ř             ř                 OnClick
SBdsnClick  TSpeedButtonSBtablesLeftTop WidthHeight
GroupIndexEnabled
Glyph.Data
j  f  BMf      v   (               đ                                      ŔŔŔ   ˙  ˙   ˙˙ ˙   ˙ ˙ ˙˙  ˙˙˙               ˙˙ř˙˙˙˙đ  DDřôDDOđ  ˙˙ř˙˙˙˙đ  DDřôDDDđ  ˙˙ř˙˙˙˙đ  DDřôDDOđ  ˙˙ř˙˙˙˙đ  DDřôDDDđ  ˙˙ř˙˙˙˙đ  DDřôDDOđ  ˙˙ř˙˙˙˙đ  DDřôDDDđ  ˙˙ř˙˙˙˙đ                                OnClickSBtablesClick  TSpeedButtonSBsqlLeft6Top WidthHeight
GroupIndexCaptionSQLEnabledOnClick
SBsqlClick    
TmDataBasemDBDataBaseNamedemoParams.StringsDSN=demo 
WaitCursor	crSQLWaitShowWaitCursorLeft0TopP  TmQuerymQuerySQL.Stringsselect * from customerwhere custno = :custno DataBasemDB
CursorTypectFwdOnlyCursorParamsDataType	ftIntegerNamecustno	ParamTypeptInputValueć  	AfterOpenmQueryAfterOpen
AfterClosemQueryAfterCloseLeft0Topp  TDataSourceDataSource1DataSetmQueryLeft0Top   TmTablemTable1DataBasemDB	AfterOpenmQueryAfterOpen
AfterClosemQueryAfterCloseLeftPTopp   