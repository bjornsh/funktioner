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
### Dilax: APR påstigande (rolling average per month for subsequent 12 months)
### dvs 2021-07 = mean APR for 2021-07 to 2022-06  
####################

funk_apr_hpl_all_month = function(con){
  fetch_dat =  dbSendQuery(con, "
SELECT hpl_id, Year, Month, Boardings = SUM(Boardings), Alightings = SUM(Alightings)
FROM
(SELECT Value AS hpl_id, Year, Month, StopPositionId, Boardings, Alightings 
  FROM [ULDataLake].[DilaxExtract].[StopProjectedDailySum]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Month]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[OperationMonthId] = [ULDataLake].[DilaxTransform].[Month].[Id]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Stop]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[StopId] = [ULDataLake].[DilaxTransform].[Stop].[Id]
  ) AS a
  GROUP BY hpl_id, Year, Month 
  ORDER BY Year, Month ")  
  
  return(dbFetch(fetch_dat))
}

# funk_apr_hpl_all_month(con = con)

####################
### Dilax: APR påstigande (rolling average for last 12 months)
### dvs 2021-07 = mean APR for 2021-07 to 2022-06  
####################


funk_apr_hpl_latest_month = function(con){
  fetch_dat =  dbSendQuery(con, "
SELECT hpl_id, Year, Month, Boardings = SUM(Boardings), Alightings = SUM(Alightings)
FROM
(SELECT Value AS hpl_id, Year, Month, StopPositionId, Boardings, Alightings 
  FROM [ULDataLake].[DilaxExtract].[StopProjectedDailySum]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Month]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[OperationMonthId] = [ULDataLake].[DilaxTransform].[Month].[Id]

  LEFT JOIN [ULDataLake].[DilaxTransform].[Stop]
  ON [ULDataLake].[DilaxExtract].[StopProjectedDailySum].[StopId] = [ULDataLake].[DilaxTransform].[Stop].[Id]
  ) AS a
  GROUP BY hpl_id, Year, Month 
  ORDER BY Year, Month ")  
  
  fetch_dat1 <- dbFetch(fetch_dat)
  
  # filter data to get latest data (rolling average per month for last 12 months)
  apr = fetch_dat1 %>% 
    filter(Month == as.numeric(format(Sys.Date(), "%m")) - 1,
           Year == as.numeric(format(Sys.Date(), "%Y")) - 1) %>% 
    arrange(hpl_id)
  
  return(apr)
}

# funk_apr_hpl_latest_month(con = con)





####################
### Atries: hämta incheckningsdata för resekort och app   
####################

funk_incheck = function(con, datum_start, datum_stop){ # datum = "YYYY-MM-DD"
  incheck = 
    # append rader från kort och app query
    rows_append(
      ## Incheckning med kort
      tbl(con, dbplyr::in_schema("AtriesExtract", "ACC_CHECKIN")) %>%
        select(datum = CHECKINDATE,
               checkin = CHECKINTIMESTAMP,
               linje = LINIE,
               hpl_id = EINSTIEG,
               ticketid = CC_SERNO_HASHED) %>%
        # filter relevant tidsperiod
        filter(between(datum, paste0(datum_start, " ", "00:00:00.000"),
                       paste0(datum_stop, " ", "00:00:00.000"))) %>%
        select(-datum) %>%
        # lägg till kolumn som identifierar origin
        mutate(typ = "kort"),
      
      ## Incheckning med app
      tbl(con, dbplyr::in_schema("BoBULExtract", "TicketEvent")) %>%
        select(checkin = DateTime,
               linje = LineNumber,
               hpl_id = StopAreaId,
               ticketid = TicketId) %>%
        filter(between(checkin, paste0(datum_start, " ", "00:00:00.000"),
                       paste0(datum_stop, " ", "00:00:00.000")))  %>%
        mutate(typ = "app")
      ) %>%
    # fetch data
    collect()
  
  return(incheck) }

# incheck = funk_incheck(con, "2022-05-30", "2022-06-01")



####################
### Atries: antal personer som byter vid en hållplats   
####################

funk_byte = function(con, datum_start, datum_stop, max_sec_between_incheck){ 
  # datum = "YYYY-MM-DD"
  # max_sec_between_incheck, eg "3600" for one hour
  
  # hämta incheckningsdata
  dat = funk_incheck(con, datum_start, datum_stop) %>% 
    mutate(datum = substr(checkin, 1, 10))
  
  # total number of boarding per hpl and day (based on incheckningsdata NOT APR)
  board = dat %>% 
    group_by(datum, hpl_id) %>% 
    summarise(boarding = n()) %>% 
    ungroup() %>% 
    filter(nchar(hpl_id) > 4)
  
  # antal dagar som ingår i urvalet
  antal_dagar = length(unique(dat$datum))
  
  
  ## remove all people with less than 2 påstigande per dygn
  id_inklud = dat %>%
    group_by(datum, ticketid) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    mutate(concat = paste(datum, ticketid)) %>% 
    select(concat) %>% pull()
  
  dat1 = dat %>% 
    mutate(concat = paste(datum, ticketid)) %>%
    filter(concat %in% id_inklud)
    
  
  ### calculate time difference between boardings with same ID on same day
  x = dat1 %>% 
    # convert to date-time
    mutate(checkin = as.POSIXct(checkin)) %>%
    # arrange in correct temporal order
    group_by(concat) %>%
    arrange(checkin, .by_group = TRUE) %>%
    ungroup() %>% 
    group_by(datum, checkin) %>% 
    # calculate time diff between second & first, third & second etc
    mutate(time_diff = checkin - lag(checkin)) %>% 
    # remove all first boardings, ie start of journey
    filter(!is.na(time_diff)) %>%
    # remove all boardings later than 1 hour after previous
    # as this is most likely a return journey and not a change to continue the journey
    filter(time_diff <= max_sec_between_incheck)
    
    
    # calculate number of people changing at a stop per date
    byte = x %>% 
      # number of changes per hpl and date
      group_by(datum, hpl_id) %>% 
      summarise(antal_byte = n()) %>%
      ungroup() %>%
      # remove unreasonable hpl_id
      filter(nchar(hpl_id) > 4)
    
    ### join byte with total boarding (based on incheckning, NOT APR)
    # hpl_id not present in df have 0 byte.
    # antal_byte = 0 and andel_byte_av_boarding = 0 needs to be added
    final = byte %>% 
      left_join(., board, by = c("hpl_id", "datum")) %>%
      # share of boarding that is a change between busses (no incheckning on trains)
      # sum across days and then divide with number of days in sample
      group_by(hpl_id) %>% 
      summarise(antal_byte = sum(antal_byte) / antal_dagar,
                boarding = sum(boarding) / antal_dagar) %>%
      # andel byte av incheckning, dvs boarding enligt Atries
      mutate(andel_byte_av_boarding = round(antal_byte / boarding, 2))
    
    return(final)
}

# byte = funk_byte(con, "2022-05-30", "2022-05-31", "3600")


####################
### Atries: hur ofta används en viss biljettyp   
####################
# [ULDataLake].[BoBULExtract].[ProductCategory]
# 
# incheck = funk_incheck(con, "2022-05-01", "2022-05-03")
# 
# datum_start = "2022-05-01"
# datum_stop = "2022-05-31"
# 
# linje_urval = c("773", "848", "874")
# linje_urval = c()
# 
# 
# # hämta incheckningsdata
# incheck = 
#     ## Incheckning med kort
#     tbl(con, dbplyr::in_schema("AtriesExtract", "ACC_CHECKIN")) %>%
#       select(datum = CHECKINDATE,
#              checkin = CHECKINTIMESTAMP,
#              linje = LINIE,
#              hpl_id = EINSTIEG,
#              ticket_no = FAHRAUSWEISART, # laggt till variabel
#              ticketid = CC_SERNO_HASHED) %>%
#       # filter relevant tidsperiod
#       filter(between(datum, paste0(datum_start, " ", "00:00:00.000"),
#                      paste0(datum_stop, " ", "00:00:00.000"))) %>%
#   # fetch data
#   collect()
# 
# 
# # hämta ticket metadata
# ticket_meta = tbl(con, dbplyr::in_schema("AtriesExtract", "TICKTAB_ALL")) %>%
#   select(ticket_namn = TICKET_NAME,
#          ticket_no = TICKET_NO,
#          rkm = VERKEHR) %>%
#   # filter relevant rkm och biljettyp
#   filter(rkm == "7",
#          ticket_namn %LIKE% '%30%') %>%
#   select(-rkm) %>% 
#   collect()
# 
# 
# # slå ihop data 
# # om ett urval av linjer filtreras tas linjerna bort annars är alla linjer med
# incheck %>% 
#   left_join(., ticket_meta, by = "ticket_no") %>%  
#   filter(!is.na(ticket_namn)) %>% 
#   filter(if (exists('linje_urval') && length(linje_urval) > 0) 
#          as.character(linje) %in% linje_urval else TRUE) %>% 
#   group_by(ticketid) %>% 
#   tally() %>%  
#   ungroup() %>% 
#   summarise(medel = mean(n),
#             median = median(n),
#             max = max(n))
# 
# 
# 
