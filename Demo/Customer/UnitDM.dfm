�
 TDM 0�	  TPF0TDMDMOldCreateOrderLeft� Top	Height� WidthD 
TmDataBase
mDataBase1Params.StringsDBQ=..\DB\demo.mdb&DRIVER=Microsoft Access Driver (*.mdb) Left,Top  TmQuerymQuery1SQL.StringsSELECT   * FROM customerorder by company ModifySQL.Stringsupdate customerset Company = :Company,Addr1 = :Addr1,Addr2 = :Addr2,City = :City,State = :State,Zip = :Zip,Country = :Country,Phone = :Phone,FAX = :FAX,TaxRate = :TaxRate,Contact = :Contact,"LastInvoiceDate = :LastInvoiceDateWhere CustNo = :OLD_CustNo InsertSQL.Stringsinsert into customer(Company,Addr1,Addr2,City,State,Zip,Country,Phone,FAX,TaxRate,Contact,LastInvoiceDate)values (	:Company,:Addr1,:Addr2,:City,:State,:Zip,	:Country,:Phone,:FAX,	:TaxRate,	:Contact,:LastInvoiceDate) DeleteSQL.Stringsdelete from customerwhereCustNo = :OLD_CustNo DataBase
mDataBase1RequestLive	
CursorTypectFwdOnlyCursorLeft� Top  TmQuerymQuery2SQL.StringsSELECT   * FROM orderswhere custno = :custno ModifySQL.Stringsupdate ordersset OrderNo = :OrderNo,CustNo = :CustNo,SaleDate = :SaleDate,ShipDate = :ShipDate,EmpNo = :EmpNo,ShipToContact = :ShipToContact,ShipToAddr1 = :ShipToAddr1,ShipToAddr2 = :ShipToAddr2,ShipToCity = :ShipToCity,ShipToState = :ShipToState,ShipToZip = :ShipToZip,ShipToCountry = :ShipToCountry,ShipToPhone = :ShipToPhone,ShipVIA = :ShipVIA,	PO = :PO,Terms = :Terms,PaymentMethod = :PaymentMethod,ItemsTotal = :ItemsTotal,TaxRate = :TaxRate,Freight = :Freight,AmountPaid = :AmountPaidWhere OrderNo = :OLD_OrderNo InsertSQL.Stringsinsert into orders(OrderNo,CustNo,	SaleDate,	ShipDate,EmpNo,ShipToContact,ShipToAddr1,ShipToAddr2,ShipToCity,ShipToState,
ShipToZip,ShipToCountry,ShipToPhone,ShipVIA,PO,Terms,PaymentMethod,ItemsTotal,TaxRate,Freight,
AmountPaid)values (	:OrderNo,:CustNo,
:SaleDate,
:ShipDate,:EmpNo,:ShipToContact,:ShipToAddr1,:ShipToAddr2,:ShipToCity,:ShipToState,:ShipToZip,:ShipToCountry,:ShipToPhone,	:ShipVIA,:PO,:Terms,:PaymentMethod,:ItemsTotal,	:TaxRate,	:Freight,:AmountPaid) DeleteSQL.Stringsdelete from orderswhereOrderNo = :OLD_OrderNo DataBase
mDataBase1
DataSourceDataSource1
CursorTypectStaticCursorParams.Data
     custno    Left� Top@  TDataSourceDataSource1DataSetmQuery1Left� Top   