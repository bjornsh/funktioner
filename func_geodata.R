

### Standardised method for creating SF object from point coordinates

create_sf <- function(df, lat, lon, crs){ # crs: WGS84 = 4326, SWEREF99 = 3006
  df %>%
    filter(!is.na( {{lon}} ), !is.na( {{lat}} )) %>% # remove rows with missing data
    # Standardise column names
    mutate( latitude  = as.numeric(gsub(",", ".", {{lat}} )),
            longitude = as.numeric(gsub(",", ".",  {{lon}}  ))) %>%
	sf::st_as_sf(coords = c("longitude", "latitude"),
                 agr = "constant",
                 crs = crs, # assign CRS
                 stringsAsFactors = FALSE,
                 remove = TRUE)
  }

# example
# create_sf(hpl_koord, hpl_lat, hpl_lon, crs = 4326) %>%
#   mapview::mapview()



### Skapa centerkoordinater för SCB rutnät data

# SCB rut ID är en kod för nedre vänster hörn av rutan
# Identifiera rutstorleken
rutid_till_centercoord <- function(df, position){ # position av kolumnen med RutID
  rutstorlek <- ifelse(max(substr(as.data.frame(df)[,position],11, 13)) == "900", 100,
                       ifelse(max(substr(as.data.frame(df)[,position],11, 13)) == "500", 500,
                              ifelse(max(substr(as.data.frame(df)[,1],11, 13)) == "000", 1000, "-999")))

  # avstånd till centerkoordinaten
  dist_to_center <- rutstorlek / 2

  # create grid center coordinates from grid ID (bottom left corner + grid diameter in x and y direction
  output <- df %>%
    as.data.frame() %>%
    dplyr::select(rut = 1, last_col()) %>%
    mutate(x_center = as.numeric(substr(rut, 1, 6)) + dist_to_center,
           y_center = as.numeric(substr(rut, 7, 13)) + dist_to_center) %>%
    st_as_sf(.,
             coords = c("x_center", "y_center"),
             agr = "constant",
             crs = 3006,        # assign SWEREF99 as CRS
             stringsAsFactors = FALSE,
             remove = TRUE
             )

  return(output)
}


# Example

# # create directory to store data
# dir.create("data")
#
# #### SCB befolkningsdata för 1km2 rutor (4.6 MB)
# download.file("https://www.scb.se/contentassets/790b7863da264730b626e4289dcb15a5/grid1km_totpop_20181231.zip",
#               destfile= "data/grid.zip")
#
# unzip("data/grid.zip", exdir="data", overwrite=TRUE)
#
# filenames <- list.files(path="data",pattern="*shp")
#
# scb = st_read(paste0("data/", filenames),
#               options = "ENCODING=WINDOWS-1252")
#
# rutid_till_centercoord(scb, 1) %>% mapview()




