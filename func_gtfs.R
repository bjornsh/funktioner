##################################################################################
### Syfte
##################################################################################

# Bra att ha funktioner för hantering av GTFS data
# laddar gtfs_obj med tidytransit::read_gtfs() 




##################################################################################
### Functions
##################################################################################

### correct spelling in whole GTFS object
# developed by: Tommy Eriksson (Uppsala kommun)
funk_spelling <- function(x) {
  for(i in 1:length(x)){
    for(j in 1:length(x[[i]])){
      if( is.character(x[[i]][[j]])){
        x[[i]][[j]] =  str_replace_all(x[[i]][[j]],
                                       c("Ã„" = "Ä",
                                         "Ã¤" = "ä",
                                         "Ã–" = "Ö",
                                         "Ã¶" = "ö",
                                         "Ã…" = "Å",
                                         "Ã¥" = "å",
                                         "Ã©" = "é",
                                         "Ã¼" = "ü"))
      }
    }
  }
  return(x)
}




### Create df med korrekt hpl name and hpl ID
funk_hpl_id_namn <- function(df){
  df$stops %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, stop_name) %>% 
    distinct()
}


### beräkna antal avgånger per hållplats inom utvalda perioden
funk_antal_departure_hpl <- function(df){
  df$routes %>%
    left_join(., df$trips, by = "route_id") %>% 
    left_join(., df$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, trip_id) %>% 
    # departures per hpl
    group_by(hpl_id) %>% 
    summarise(antal_departure = n())
}



### beräkna antal linjer per hållplats inom utvalda perioden
funk_antal_linjer_hpl <- function(df){
  df$routes %>% left_join(., df$trips, by = "route_id") %>% 
    left_join(., df$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, route_short_name) %>%
    # remove duplicates
    distinct() %>% 
    # lines per hpl
    group_by(hpl_id) %>% 
    summarise(antal_linjer = n())
}


### Alla linjer per hållplats
funk_linjer_hpl = function(df){
  df$routes %>% left_join(., df$trips, by = "route_id") %>% 
    left_join(., df$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, route_short_name) %>%
    # remove duplicates
    distinct() %>% 
    # lines per hpl
    group_by(hpl_id) %>% 
    summarise(linjer = paste(route_short_name, collapse = ","))
}




### Create SF object with one coordinate per hpl instead of one coordinate per hållplatsläge
funk_hpl_koordinat <- function(df){
  
  df$stops %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, stop_lat, stop_lon) %>% 
    # create one point per hpl
    group_by(hpl_id) %>% 
    summarise(stop_lat = mean(stop_lat), 
              stop_lon = mean(stop_lon)) %>% 
    # skapa SF object
    st_as_sf(
      coords = c("stop_lon", "stop_lat"),
      agr = "constant",
      crs = 4326,        # assign WGS84 as CRS
      stringsAsFactors = FALSE,
      remove = TRUE
    ) 
}



# create SF with most common shape per line
funk_linje_network <- function(df){
  
  # identify most common shape, ie route, per line
  mest_frekvent_shape <- df$trips %>% 
    left_join(., df$routes, by = "route_id") %>% 
    # calculate number of trips per route and line
    group_by(route_short_name, shape_id) %>% 
    summarise(n = n()) %>% 
    # remove all routes except the most common
    filter(n == max(n)) %>% 
    # in case both directions have same number of journeys, remove one 
    ungroup() %>% 
    group_by(route_short_name) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    # remove variable
    select(-n)
  
  # create linestring for each line
  df$shapes %>% 
    # remove all routes that are not the most common per line
    filter(shape_id %in% mest_frekvent_shape$shape_id) %>% 
    # create SF linestring
    shapes_as_sf(., crs = 4326) %>% 
    # add line name
    left_join(., mest_frekvent_shape, by = "shape_id")
}


# filter gtfs_obj by trips related to certain line types, eg remove all trips, routes etc for skolbussar
funk_remove_linjetyp <- function(df, remove_linjetyp){
  route_trip_include = df$routes %>% 
    left_join(., df$trips, by = "route_id") %>% 
    filter(route_desc %notin% remove_linjetyp) %>%
    select(trip_id) %>% 
    distinct()
  
  filter_feed_by_trips(df, unique(route_trip_include$trip_id)) 
  }


