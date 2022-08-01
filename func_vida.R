##################################################################################
### Syfte
##################################################################################

### Bra att ha funktioner för hantering av UL:s SQL databas

### Det krävs: 
# VPN uppkoppling 
# R paketet "odbc" och 
# att man är ansluten till databasen: 
# con <- dbConnect(odbc(),
# Driver = "SQL Server",
# Server = "SKRIVA: SERVER",
# Database = "ULDataLake",
# UID = "SKRIVA: INLOGGNINGS ID",
# PWD = "SKRIVA: LÖSENORD")



##################################################################################
### Functions
##################################################################################




####################
### APR påstigande (rolling average per month for subsequent 12 months)
### dvs 2021-07 = mean APR for 2021-07 to 2022-06  
####################

funk_apr_hpl_all_month = function(con){
  fetch_dat =  dbSendQuery(con, "
SELECT hplid, Year, Month, Boardings = SUM(Boardings), Alightings = SUM(Alightings)
FROM
(SELECT hplid = Value, Year, Month, StopPositionId, Boardings, Alightings 
  FROM [ULDataLake].[DilaxExtract].[StopProjectedDailySum]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Month]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[OperationMonthId] = [ULDataLake].[DilaxTransform].[Month].[Id]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Stop]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[StopId] = [ULDataLake].[DilaxTransform].[Stop].[Id]
  ) AS a
  GROUP BY hplid, Year, Month 
  ORDER BY Year, Month ")  
  
  return(dbFetch(fetch_dat))
}

# funk_apr_hpl_all_month(con = con)

####################
### APR påstigande (rolling average for last 12 months)
### dvs 2021-07 = mean APR for 2021-07 to 2022-06  
####################


funk_apr_hpl_latest_month = function(con){
  fetch_dat =  dbSendQuery(con, "
SELECT hplid, Year, Month, Boardings = SUM(Boardings), Alightings = SUM(Alightings)
FROM
(SELECT hplid = Value, Year, Month, StopPositionId, Boardings, Alightings 
  FROM [ULDataLake].[DilaxExtract].[StopProjectedDailySum]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Month]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[OperationMonthId] = [ULDataLake].[DilaxTransform].[Month].[Id]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Stop]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[StopId] = [ULDataLake].[DilaxTransform].[Stop].[Id]
  ) AS a
  GROUP BY hplid, Year, Month 
  ORDER BY Year, Month ")  
  
  fetch_dat1 <- dbFetch(fetch_dat)
  
  # filter data to get latest data (rolling average per month for last 12 months)
  apr = fetch_dat1 %>% 
    filter(Month == as.numeric(format(Sys.Date(), "%m")) - 1,
           Year == as.numeric(format(Sys.Date(), "%Y")) - 1) %>% 
    arrange(hplid)
  
  return(apr)
}

# funk_apr_hpl_latest_month(con = con)
