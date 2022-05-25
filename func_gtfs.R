##################################################################################
### Syfte
##################################################################################

# Bra att ha funktioner för hantering av GTFS data
# laddar gtfs_obj med tidytransit::read_gtfs() 




##################################################################################
### Functions
##################################################################################


####################
### correct spelling in whole GTFS object
### developed by: Tommy Eriksson (Uppsala kommun)
####################

funk_spelling <- function(gtfs_obj) {
  for(i in 1:length(gtfs_obj)){
    for(j in 1:length(gtfs_obj[[i]])){
      if( is.character(gtfs_obj[[i]][[j]])){
        gtfs_obj[[i]][[j]] =  str_replace_all(gtfs_obj[[i]][[j]],
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


####################
### Create df med korrekt hpl name and hpl ID
####################

funk_hpl_id_namn <- function(df){
  df$stops %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13))) %>% 
    select(hpl_id, stop_name) %>% 
    distinct()
}


####################
### Beräkna antal avgånger per hållplats
####################

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


####################
### Beräkna antal linjer per hållplats
####################

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


####################
### Alla linjer per hållplats
####################

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


####################
### Create SF object with one coordinate per hpl instead of one coordinate per hållplatsläge
####################

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


####################
### Create SF with most common shape per line
####################
 
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


####################
### Filter gtfs_obj by trips related to certain line types, eg remove all trips, routes etc for skolbussar
####################

funk_remove_linjetyp <- function(df, remove_linjetyp){
  route_trip_include = df$routes %>% 
    left_join(., df$trips, by = "route_id") %>% 
    filter(route_desc %notin% remove_linjetyp) %>%
    select(trip_id) %>% 
    distinct()
  
  filter_feed_by_trips(df, unique(route_trip_include$trip_id)) 
  }


####################
### Calculate overlap between all combinations of lines using a stop
####################

funk_linje_overlap = function(gtfs_obj, which_hpl, buffer_meter) {
  # identify all lines using a certain hpl
  hpl_linje_filtered = funk_linjer_hpl(gtfs_obj) %>% filter(hpl_id == which_hpl)
  
  # create character string with all lines using a certain hpl
  hpl_linje_filtered_vector = as.vector(str_split(hpl_linje_filtered$linjer, ",", n = Inf, simplify = FALSE)[[1]])
  
  # create frame with all line combinations for a hpl
  frame = merge(hpl_linje_filtered_vector, hpl_linje_filtered_vector) %>% 
    # remove duplicates
    filter(x != y) %>% 
    # remove duplicates in other direction
    mutate(concat = ifelse(x < y, paste0(x, "_", y), paste0(y, "_", x))) %>%
    distinct(concat, .keep_all = TRUE) %>%
    select(-concat) 
  
  # create line network for selected lines
  selected_lines_sf = funk_linje_network(gtfs_obj) %>%
    filter(route_short_name %in% hpl_linje_filtered_vector)

  # create buffer around each line
  selected_lines_buff = selected_lines_sf %>%
    st_transform(3006) %>%
    st_buffer(., buffer_meter) %>%
    st_make_valid() 
  
  # calculate buffered area for each line
  area_linje = selected_lines_sf %>%
    st_transform(3006) %>%
    st_buffer(., buffer_meter) %>%
    st_make_valid() %>%
    mutate(area_linje = st_area(.)) %>%
    as.data.frame() %>%
    select(route_short_name, area_linje)
  
  
  ### loop through all line combinations and intersect körsträckor
  xxx = c()
  
  for(i in 1:nrow(frame)) {
    xxx = rbind(xxx, st_intersection(filter(selected_lines_buff, route_short_name == frame[i,1]),
                                     filter(selected_lines_buff, route_short_name == frame[i,2])))
  }
  
  # create area of overlap for each line combination
  resultat = xxx %>%
    rename(linje1 = route_short_name.1,
           linje2 = route_short_name) %>%
    # calculate size of overlapping area for first line
    mutate(area_overlap = st_area(geometry)) %>%
    left_join(., area_linje, by = c("linje1" = "route_short_name")) %>%
    rename(area_linje1 = area_linje) %>%
    # calculate size of overlapping area for second line
    left_join(., area_linje, by = c("linje2" = "route_short_name")) %>%
    rename(area_linje2 = area_linje) %>%
    # convert to df
    as.data.frame() %>%
    # calculate share of körsträcka that overlap with other line
    mutate(andel_overlap_linje1 = round(as.numeric(area_overlap / area_linje1), 3),
           andel_overlap_linje2 = round(as.numeric(area_overlap / area_linje2), 3)) %>%
    mutate(mean_overlap = round((andel_overlap_linje1 + andel_overlap_linje2) / 2, 3)) %>%
    # create output
    select(linje1, linje2,
           area_linje1, area_linje2,
           area_overlap,
           andel_overlap_linje1, andel_overlap_linje2, mean_overlap) %>%
    # add hpl id so multiple df for different hpl can be appended
    mutate(hpl_id = which_hpl)
  
  print(resultat)
}
