/* Microsoft SQL Server - Scripting			*/
/* Server: ALTA_SQL					*/
/* Database: AltaDataBase					*/
/* Creation Date 11/17/98 13:16:12 			*/

set quoted_identifier on
GO

/****** Object:  Stored Procedure dbo.AddSampleStock    Script Date: 11/17/98 13:16:16 ******/
if exists (select * from sysobjects where id = object_id('dbo.AddSampleStock') and sysstat & 0xf = 4)
	drop procedure "dbo"."AddSampleStock"
GO

/****** Object:  Table dbo.StockSample    Script Date: 11/17/98 13:16:16 ******/
if exists (select * from sysobjects where id = object_id('dbo.StockSample') and sysstat & 0xf = 3)
	drop table "dbo"."StockSample"
GO

/****** Object:  Table dbo.StockSample    Script Date: 11/17/98 13:16:16 ******/
CREATE TABLE "dbo"."StockSample" (
	"ID" "int" IDENTITY (1, 1) NOT NULL ,
	"NikName" varchar (255) NULL ,
	CONSTRAINT "PK___5__12" PRIMARY KEY  CLUSTERED 
	(
		"ID"
	)
)
GO

/****** Object:  Stored Procedure dbo.AddSampleStock    Script Date: 11/17/98 13:16:17 ******/
CREATE PROCEDURE AddSampleStock( @parNikName char(255) ) AS

INSERT INTO StockSample ( NikName )
VALUES ( @parNikName )
GO

