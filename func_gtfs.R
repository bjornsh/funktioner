##################################################################################
### Syfte
##################################################################################

# Bra att ha funktioner för hantering av GTFS data
# för att ladda ner GTFS data från Trafiklabs API behövs det en API nyckel till GTFS Regional Static data
# https://developer.trafiklab.se/api/gtfs-regional-static-data

# laddar gtfs_obj med tidytransit::read_gtfs() 
# för rätt encoding source funktioner med: eval(parse("https://raw.githubusercontent.com/bjornsh/funktioner/main/func_gtfs.R", encoding="UTF-8"))



##################################################################################
### Functions
##################################################################################

####################
### Table with RKM codes used by Trafiklab
####################

funk_gtfs_rkm_kod = function(){
  lan_kod <- c("01", "03", "04", "05", "06", "07", "08", "09", "10",
               "12", "13", "14", "17", "18", "19",
               "20", "21", "22", "23", "24", "25", "")
  region <- c("Stockholm", "Uppsala", "Södermanland", "Östergötland", "Jönköping",
              "Kronoberg", "Kalmar", "Gotland", "Blekinge", "Skåne", "Halland", "Västra Götaland",
              "Värmland", "Örebro", "Västmanland", "Dalarna", "Gävleborg", "Västernorrland",
              "Jämtland", "Västerbotten", "Norrbotten", "SJ")
  rkm <- c("sl", "ul", "sormland", "otraf", "SAKNAS", "krono", "klt", "gotland", "blekinge", "skane",
           "halland", "vt", "varm", "orebro", "vl", "dt", "xt", "dintur", "SAKNAS", "SAKNAS", "SAKNAS", "sj")
  
  data.frame(lan_kod, region, rkm)
}

# funk_gtfs_rkm_kod()




####################
### Download today's GTFS zip file for specific region
####################

funk_gtfs_download <- function(rkm){
  # create folder to store GTFS zip file
  dir.create(file.path(getwd(), "gtfs_data"))
  
  # define GTFS area
  rkm <- rkm
  
  # API key
  trafiklab_key = rstudioapi::askForPassword()
  
  # define API url 
  url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", trafiklab_key)
  
  # download GTFS zip file from API and store in folder "wd/gtfs_data" 
  GET(url, 
      write_disk(file.path(getwd(), "gtfs_data", paste0("gtfs_", rkm, "_", Sys.Date(), ".zip")), 
                 overwrite=TRUE))
}

# funk_gtfs_download(rkm = "ul")



####################
### Download today's GTFS zip file for specific region and 
### read file into R with tidytransit::read_gtfs
####################

funk_gtfs_download_read <- function(rkm){
  # create folder to store GTFS zip file
  dir.create(file.path(getwd(), "gtfs_data"))
  
  # define GTFS area
  rkm <- rkm
  
  # API key
  trafiklab_key = rstudioapi::askForPassword()
  
  # define API url 
  url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", trafiklab_key)
  
  # download GTFS zip file from API and store in folder "wd/gtfs_data" 
  GET(url, 
      write_disk(file.path(getwd(), "gtfs_data", paste0("gtfs_", rkm, "_", Sys.Date(), ".zip")), 
                 overwrite=TRUE))
  
  # read gtfs data with tidytransit
  read_gtfs(file.path(getwd(), "gtfs_data", paste0("gtfs_", rkm, "_", Sys.Date(), ".zip")))
}

# test = funk_gtfs_download_read(rkm = "ul")


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
  return(gtfs_obj)
}


####################
### Skapa df med alla hållplatslägen, läges ID och koordinater
### Eftersom GTFS innehåller historiskt data måste GTFS filtreras för ett specifikt datum
####################

funk_hpl_id_lage <- function(gtfs_obj, datum){ # datum: "YYYY-MM-DD"
  gtfs_obj %>% 
    # filter date
    filter_feed_by_date(., datum) %>%
    # select stops table
    .[2] %>%
    # convert from list to df
    as.data.frame() %>% 
    # create hpl and hpl läge ID
    mutate(hpl_id = substr(stops.stop_id, 8, 13),
           hpl_id_lage = paste0(substr(stops.stop_id, 8, 13), "_", stops.platform_code)) %>%
    select(hpl_namn = stops.stop_name,
           hpl_id,
           hpl_id_lage,
           lat = stops.stop_lat,
           lon = stops.stop_lon) 
}

# funk_hpl_id_lage(dat, "2022-09-28")


####################
### Skapa df med alla hållplatser, dvs unika stop_name och stop_id
### Eftersom GTFS innehåller historiskt data måste GTFS filtreras för ett specifikt datum
####################

funk_hpl_id <- function(gtfs_obj, datum){ # datum: "YYYY-MM-DD"
  funk_hpl_id_lage(gtfs_obj, datum) %>%
    group_by(hpl_namn, hpl_id) %>%
    summarise(lat = mean(lat),
              lon = mean(lon))
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
### Beräkna antal avgånger per timma och linje för varje hållplats 
####################

antal_departure_per_hpl_line_hr = function(df){
  df$routes %>%
    left_join(., df$trips, by = "route_id") %>% 
    left_join(., df$stop_times, by = "trip_id") %>% 
    # create hpl_id
    mutate(hpl_id = as.integer(substr(stop_id, 8, 13)),
           dep_hr = substr(departure_time,1,2)) %>% 
    select(hpl_id, route_short_name, dep_hr) %>% 
    group_by(hpl_id, route_short_name, dep_hr) %>% 
    tally()
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
