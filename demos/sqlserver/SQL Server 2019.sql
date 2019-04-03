/* 
Name: SQL Server 2019.sql
Purpose: Shows the Query Improvements in SQL Server 2019.
Author: Buck Woody, MSFT. All rights reserved. Demo purposes only.
Requires: Wide World Importers OLTP Database, Wide World Importers DW Database, New York City Taxi Data, and a simple PII DB with three columns.
*/

-- Query Processing improvements
-- Note: Include the Actual Execution Plan before you run this query.

USE [master]
GO

-- Change behavior simply by setting the compat level. Then dump the cache to have a clean slate.
ALTER DATABASE [WideWorldImportersDW] SET COMPATIBILITY_LEVEL = 130;
GO
ALTER DATABASE SCOPED CONFIGURATION CLEAR PROCEDURE_CACHE;
GO

USE [WideWorldImportersDW];
GO

/* 
A fairly complex Table-Valued Function exists in this database: 

CREATE FUNCTION [Fact].[WhatIfOutlierEventQuantity](@event VARCHAR(15), @beginOrderDateKey DATE, @endOrderDateKey DATE)
RETURNS @OutlierEventQuantity TABLE (
	[Order Key] [bigint],
	[City Key] [int] NOT NULL,
	[Customer Key] [int] NOT NULL,
	[Stock Item Key] [int] NOT NULL,
	[Order Date Key] [date] NOT NULL,
	[Picked Date Key] [date] NULL,
	[Salesperson Key] [int] NOT NULL,
	[Picker Key] [int] NULL,
	[OutlierEventQuantity] [int] NOT NULL)
AS 
BEGIN

-- Valid @event values
	-- 'Mild Recession'
	-- 'Hurricane - South Atlantic'
	-- 'Hurricane - East South Central'
	-- 'Hurricane - West South Central'
	IF @event = 'Mild Recession'
    INSERT  @OutlierEventQuantity
	SELECT [o].[Order Key], [o].[City Key], [o].[Customer Key],
           [o].[Stock Item Key], [o].[Order Date Key], [o].[Picked Date Key],
           [o].[Salesperson Key], [o].[Picker Key], 
           CASE
			WHEN [o].[Quantity] > 2 THEN [o].[Quantity] * .5
			ELSE [o].[Quantity]
		   END 
	FROM [Fact].[Order] AS [o]
	INNER JOIN [Dimension].[City] AS [c]
		ON [c].[City Key] = [o].[City Key]

	IF @event = 'Hurricane - South Atlantic'
    INSERT  @OutlierEventQuantity
	SELECT [o].[Order Key], [o].[City Key], [o].[Customer Key],
           [o].[Stock Item Key], [o].[Order Date Key], [o].[Picked Date Key],
           [o].[Salesperson Key], [o].[Picker Key], 
           CASE
			WHEN [o].[Quantity] > 10 THEN [o].[Quantity] * .5
			ELSE [o].[Quantity]
		   END 
	FROM [Fact].[Order] AS [o]
	INNER JOIN [Dimension].[City] AS [c]
		ON [c].[City Key] = [o].[City Key]
	WHERE [c].[State Province] IN
	('Florida', 'Georgia', 'Maryland', 'North Carolina',
	'South Carolina', 'Virginia', 'West Virginia',
	'Delaware')
	AND [o].[Order Date Key] BETWEEN @beginOrderDateKey AND @endOrderDateKey

	IF @event = 'Hurricane - East South Central'
    INSERT  @OutlierEventQuantity
	SELECT [o].[Order Key], [o].[City Key], [o].[Customer Key],
           [o].[Stock Item Key], [o].[Order Date Key], [o].[Picked Date Key],
           [o].[Salesperson Key], [o].[Picker Key], 
           CASE
			WHEN [o].[Quantity] > 50 THEN [o].[Quantity] * .5
			ELSE [o].[Quantity]
		   END
	FROM [Fact].[Order] AS [o]
	INNER JOIN [Dimension].[City] AS [c]
		ON [c].[City Key] = [o].[City Key]
	INNER JOIN [Dimension].[Stock Item] AS [si]
	ON [si].[Stock Item Key] = [o].[Stock Item Key]
	WHERE [c].[State Province] IN
	('Alabama', 'Kentucky', 'Mississippi', 'Tennessee')
	AND [si].[Buying Package] = 'Carton'
	AND [o].[Order Date Key] BETWEEN @beginOrderDateKey AND @endOrderDateKey

	IF @event = 'Hurricane - West South Central'
    INSERT  @OutlierEventQuantity
	SELECT [o].[Order Key], [o].[City Key], [o].[Customer Key],
           [o].[Stock Item Key], [o].[Order Date Key], [o].[Picked Date Key],
           [o].[Salesperson Key], [o].[Picker Key], 
           CASE
		    WHEN [cu].[Customer] = 'Unknown' THEN 0
			WHEN [cu].[Customer] <> 'Unknown' AND
			 [o].[Quantity] > 10 THEN [o].[Quantity] * .5
			ELSE [o].[Quantity]
		   END
	FROM [Fact].[Order] AS [o]
	INNER JOIN [Dimension].[City] AS [c]
		ON [c].[City Key] = [o].[City Key]
	INNER JOIN [Dimension].[Customer] AS [cu]
	ON [cu].[Customer Key] = [o].[Customer Key]
	WHERE [c].[State Province] IN
	('Arkansas', 'Louisiana', 'Oklahoma', 'Texas')
	AND [o].[Order Date Key] BETWEEN @beginOrderDateKey AND @endOrderDateKey

    RETURN
END
GO
*/

-- This query uses that TVF in a join:
SELECT  [fo].[Order Key], [fo].[Description], [fo].[Package],
		[fo].[Quantity], [foo].[OutlierEventQuantity]
FROM    [Fact].[Order] AS [fo]
INNER JOIN [Fact].[WhatIfOutlierEventQuantity]('Mild Recession',
                            '1-01-2013',
                            '10-15-2014') AS [foo] ON [fo].[Order Key] = [foo].[Order Key]
                            AND [fo].[City Key] = [foo].[City Key]
                            AND [fo].[Customer Key] = [foo].[Customer Key]
                            AND [fo].[Stock Item Key] = [foo].[Stock Item Key]
                            AND [fo].[Order Date Key] = [foo].[Order Date Key]
                            AND [fo].[Picked Date Key] = [foo].[Picked Date Key]
                            AND [fo].[Salesperson Key] = [foo].[Salesperson Key]
                            AND [fo].[Picker Key] = [foo].[Picker Key]
INNER JOIN [Dimension].[Stock Item] AS [si] 
	ON [fo].[Stock Item Key] = [si].[Stock Item Key]
WHERE   [si].[Lead Time Days] > 0
		AND [fo].[Quantity] > 50;

-- Query Plan observations:
--		Notice the TVF estimated number of rows
--		Notice the spills 


-- Next, open separate query window to compare plans
-- Our "after" state (with Interleaved execution) set only by changing the compat level
-- Include Actual Execution Plan
USE [master];
GO
ALTER DATABASE [WideWorldImportersDW] SET COMPATIBILITY_LEVEL = 150;
GO
ALTER DATABASE SCOPED CONFIGURATION CLEAR PROCEDURE_CACHE;
GO

-- Run that same query again, referencing the complex TVF:
USE [WideWorldImportersDW];
GO

SELECT  [fo].[Order Key], [fo].[Description], [fo].[Package],
		[fo].[Quantity], [foo].[OutlierEventQuantity]
FROM    [Fact].[Order] AS [fo]
INNER JOIN [Fact].[WhatIfOutlierEventQuantity]('Mild Recession',
                            '1-01-2013',
                            '10-15-2014') AS [foo] ON [fo].[Order Key] = [foo].[Order Key]
                            AND [fo].[City Key] = [foo].[City Key]
                            AND [fo].[Customer Key] = [foo].[Customer Key]
                            AND [fo].[Stock Item Key] = [foo].[Stock Item Key]
                            AND [fo].[Order Date Key] = [foo].[Order Date Key]
                            AND [fo].[Picked Date Key] = [foo].[Picked Date Key]
                            AND [fo].[Salesperson Key] = [foo].[Salesperson Key]
                            AND [fo].[Picker Key] = [foo].[Picker Key]
INNER JOIN [Dimension].[Stock Item] AS [si] 
	ON [fo].[Stock Item Key] = [si].[Stock Item Key]
WHERE   [si].[Lead Time Days] > 0
		AND [fo].[Quantity] > 50;

-- Plan observations:
--		Notice the TVF estimated number of rows (did it change?)
--		Any spills?

-- Security - Show Data Classification

-- Security - Show the Vulnerability report

/* Machine Learning
Assume you are an ride-share driver, and want to predict if you'll get a tip from a ride.
Using a sample set of data from New York City taxis, we'll build a model that uses the previous
tippers to find people who might tip. 

We'll create, train, store and even score that model (in bulk) right inside SQL Server.
*/

-- enable R
EXEC sp_configure 'external scripts enabled', 1;
RECONFIGURE WITH OVERRIDE;  
GO

-- Profile the Data in T-SQL
USE [NYC]
GO
SELECT DISTINCT [passenger_count]
    , ROUND (SUM ([fare_amount]),0) as TotalFares
    , ROUND (AVG ([fare_amount]),0) as AvgFares
FROM [dbo].[nyctaxi_sample]
GROUP BY [passenger_count]
ORDER BY  AvgFares DESC;
GO

-- User-defined function that calculates the direct distance between two 
-- geographical coordinates.
CREATE FUNCTION [dbo].[fnCalculateDistance] (@Lat1 float, @Long1 float, @Lat2 float, @Long2 float)  
  
RETURNS float  
AS  
BEGIN  
  DECLARE @distance decimal(28, 10)  
  -- Convert to radians  
  SET @Lat1 = @Lat1 / 57.2958  
  SET @Long1 = @Long1 / 57.2958  
  SET @Lat2 = @Lat2 / 57.2958  
  SET @Long2 = @Long2 / 57.2958  
  -- Calculate distance  
  SET @distance = (SIN(@Lat1) * SIN(@Lat2)) + (COS(@Lat1) * COS(@Lat2) * COS(@Long2 - @Long1))  
  --Convert to miles  
  IF @distance <> 0  
  BEGIN  
    SET @distance = 3958.75 * ATAN(SQRT(1 - POWER(@distance, 2)) / @distance);  
  END  
  RETURN @distance  
END
GO

-- Feature Engineering
CREATE FUNCTION [dbo].[fnEngineerFeatures] (  
@passenger_count int = 0,  
@trip_distance float = 0,  
@trip_time_in_secs int = 0,  
@pickup_latitude float = 0,  
@pickup_longitude float = 0,  
@dropoff_latitude float = 0,  
@dropoff_longitude float = 0)  
RETURNS TABLE  
AS
  RETURN
  (
  -- Add the SELECT statement with parameter references here
  SELECT
    @passenger_count AS passenger_count,
    @trip_distance AS trip_distance,
    @trip_time_in_secs AS trip_time_in_secs,
    [dbo].[fnCalculateDistance](@pickup_latitude, @pickup_longitude, @dropoff_latitude, @dropoff_longitude) AS direct_distance

  )
GO

-- Let's get the tips based on that geo distance

SELECT tipped, fare_amount, passenger_count,(trip_time_in_secs/60) as TripMinutes,
    trip_distance, pickup_datetime, dropoff_datetime,
    dbo.fnCalculateDistance(pickup_latitude, pickup_longitude,  dropoff_latitude, dropoff_longitude) AS direct_distance
    FROM nyctaxi_sample
    WHERE pickup_longitude != dropoff_longitude and pickup_latitude != dropoff_latitude and trip_distance = 0
    ORDER BY trip_time_in_secs DESC

-- Now we'll train a model with Logistic Regression and store that in the database itself.
CREATE PROCEDURE [dbo].[RxTrainLogitModel] (@trained_model varbinary(max) OUTPUT)

AS
BEGIN
  DECLARE @inquery nvarchar(max) = N'
    select tipped, fare_amount, passenger_count,trip_time_in_secs,trip_distance,
    pickup_datetime, dropoff_datetime,
    dbo.fnCalculateDistance(pickup_latitude, pickup_longitude,  dropoff_latitude, dropoff_longitude) as direct_distance
    from nyctaxi_sample
    tablesample (70 percent) repeatable (98052)
'

  EXEC sp_execute_external_script @language = N'R',
                                  @script = N'
## Create model
logitObj <- rxLogit(tipped ~ passenger_count + trip_distance + trip_time_in_secs + direct_distance, data = InputDataSet)
summary(logitObj)

## Serialize model 
trained_model <- as.raw(serialize(logitObj, NULL));
',
  @input_data_1 = @inquery,
  @params = N'@trained_model varbinary(max) OUTPUT',
  @trained_model = @trained_model OUTPUT; 
END
GO

DECLARE @model VARBINARY(MAX);
EXEC RxTrainLogitModel @model OUTPUT;
INSERT INTO nyc_taxi_models (name, model) VALUES('RxTrainLogit_model', @model);

-- And we can now score predictions using the binary model
CREATE PROCEDURE [dbo].[RxPredict] (@model varchar(250), @inquery nvarchar(max))
AS 
BEGIN 

DECLARE @lmodel2 varbinary(max) = (SELECT model FROM nyc_taxi_models WHERE name = @model);  
EXEC sp_execute_external_script @language = N'R',
  @script = N' 
    mod <- unserialize(as.raw(model)); 
    print(summary(mod)) 
    OutputDataSet<-rxPredict(modelObject = mod, data = InputDataSet, outData = NULL, predVarNames = "Score", type = "response", writeModelVars = FALSE, overwrite = TRUE); 
    str(OutputDataSet) 
    print(OutputDataSet) 
    ', 
  @input_data_1 = @inquery, 
  @params = N'@model varbinary(max)',
  @model = @lmodel2 
  WITH RESULT SETS ((Score float));
END
GO

-- Grab a random sample of passengers data
SELECT TOP 10 a.passenger_count AS passenger_count, a.trip_time_in_secs AS trip_time_in_secs, a.trip_distance AS trip_distance, a.dropoff_datetime AS dropoff_datetime, dbo.fnCalculateDistance(pickup_latitude, pickup_longitude, dropoff_latitude,dropoff_longitude) AS direct_distance
FROM (SELECT medallion, hack_license, pickup_datetime, passenger_count,trip_time_in_secs,trip_distance, dropoff_datetime, pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude FROM nyctaxi_sample)a
LEFT OUTER JOIN
(SELECT medallion, hack_license, pickup_datetime FROM nyctaxi_sample TABLESAMPLE (70 percent) REPEATABLE (98052)    )b
ON a.medallion=b.medallion AND a.hack_license=b.hack_license 
AND a.pickup_datetime=b.pickup_datetime
WHERE b.medallion IS NULL

-- Create a batch of scoring runs
CREATE PROCEDURE [dbo].[RxPredictBatchOutput] (@model varchar(250), @inquery nvarchar(max))
AS
BEGIN
DECLARE @lmodel2 varbinary(max) = (SELECT model FROM nyc_taxi_models WHERE name = @model);
EXEC sp_execute_external_script 
  @language = N'R',
  @script = N'
    mod <- unserialize(as.raw(model));
    print(summary(mod))
    OutputDataSet<-rxPredict(modelObject = mod, data = InputDataSet, outData = NULL, predVarNames = "Score", type = "response", writeModelVars = FALSE, overwrite = TRUE);
    str(OutputDataSet)
    print(OutputDataSet)
  ',
  @input_data_1 = @inquery,
  @params = N'@model varbinary(max)',
  @model = @lmodel2
  WITH RESULT SETS ((Score float));
END

-- Define the input data
DECLARE @query_string nvarchar(max)
SET @query_string='SELECT TOP 10 a.passenger_count as passenger_count, a.trip_time_in_secs AS trip_time_in_secs, a.trip_distance AS trip_distance, a.dropoff_datetime AS dropoff_datetime, dbo.fnCalculateDistance(pickup_latitude, pickup_longitude, dropoff_latitude,dropoff_longitude) AS direct_distance FROM  (SELECT medallion, hack_license, pickup_datetime, passenger_count,trip_time_in_secs,trip_distance, dropoff_datetime, pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude FROM nyctaxi_sample  )a   LEFT OUTER JOIN (SELECT medallion, hack_license, pickup_datetime FROM nyctaxi_sample TABLESAMPLE (70 percent) REPEATABLE (98052))b ON a.medallion=b.medallion AND a.hack_license=b.hack_license AND a.pickup_datetime=b.pickup_datetime WHERE b.medallion is null'

-- Call the stored procedure for scoring and pass the input data
EXEC [dbo].[RxPredictBatchOutput] @model = 'RxTrainLogit_model', @inquery = @query_string;

/* EOF */