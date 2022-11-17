

### Standardised method for creating SF object from point coordinates

create_sf <- function(df, lat, lon, crs){ # crs: WGS84 = 4326, SWEREF99 = 3006
  df %>%
    filter(!is.na( {{lon}} ), !is.na( {{lat}} ),
           {{lon}} != "", {{lat}} != "") %>% # remove rows with missing data
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


#### calculate summary per kommun and deso type based on Deso ID
summary_kommun_desotype <- function(df, deso_kolumn, value_kolumn, ag_fct){
  # deso_kolumn: column with Deso ID
  # value_kolumn: column to be summarised
  # ag_fct: function used to aggregate data, eg mean, median
  
  # kommun summary
  t_kommun <- df %>% 
    filter(!is.na({{value_kolumn}}), !is.na({{deso_kolumn}})) %>%
    mutate(value_kolumn = as.numeric({{value_kolumn}})) %>%
    mutate(kommun_kod = substr({{deso_kolumn}}, 1, 4),
           tatort = ifelse(substr({{deso_kolumn}}, 5, 5) == "A", "landsbygd",
                           ifelse(substr({{deso_kolumn}}, 5, 5) == "B", "tatort", "centralort"))) %>%
    left_join(., scb_kod, by = c("kommun_kod" = "Code")) %>%
    rename(kommun_namn = Name) %>%
    group_by(kommun_namn) %>%
    summarise(resultat = ag_fct(value_kolumn, na.rm = TRUE)) 
	
	# Desotyp summary
  t_desotyp <- df %>%
    filter(!is.na({{value_kolumn}}), !is.na({{deso_kolumn}})) %>%
    mutate(value_kolumn = as.numeric({{value_kolumn}})) %>%
    mutate(kommun_kod = substr({{deso_kolumn}}, 1, 4),
           tatort = ifelse(substr({{deso_kolumn}}, 5, 5) == "A", "landsbygd",
                           ifelse(substr({{deso_kolumn}}, 5, 5) == "B", "tatort", "centralort"))) %>%
    left_join(., scb_kod, by = c("kommun_kod" = "Code")) %>%
    rename(kommun_namn = Name) %>%
    group_by(kommun_namn, tatort) %>%
    summarise(resultat = ag_fct(value_kolumn, na.rm = TRUE)) %>%
    pivot_wider(names_from = tatort, values_from = resultat) %>%
    select(kommun_namn, centralort, tatort, landsbygd)

# combine kommun and deso type summaries
  t_slut <- left_join(t_kommun, t_desotyp, by = "kommun_namn") %>%
    rename(Kommun = 1, 'Hela kommun' = 2, Centralort = 3, Tätort = 4, Landsbygd = 5)
  
  return(t_slut)
  }

# Example

# summary_deso_typ(df, kolumn1, kolumn2, mean)
# summary_deso_typ(df, kolumn1, kolumn2, median)
