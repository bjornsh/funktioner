##################################################################################
### Syfte
##################################################################################

# Bra att ha funktioner för hantering av GTFS data




##################################################################################
### Functions
##################################################################################

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
    left_join(., gtfs$trips, by = "route_id") %>% 
    left_join(., gtfs$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, trip_id) %>% 
    # departures per hpl
    group_by(hpl_id) %>% 
    summarise(antal_departure = n())
}



### beräkna antal linjer per hållplats inom utvalda perioden
funk_antal_linjer_hpl <- function(df){
  df$routes %>%left_join(., gtfs$trips, by = "route_id") %>% 
    left_join(., gtfs$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, route_short_name) %>%
    # remove duplicates
    distinct() %>% 
    # lines per hpl
    group_by(hpl_id) %>% 
    summarise(antal_linjer = n())
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
    ungroup() %>% 
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