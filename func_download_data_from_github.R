##################################################################################
### Syfte
##################################################################################

# Download geodata from Github and 
# load the data into R using sf::st_read()
# without storing a local file

# source functions with: 
# eval(parse("https://raw.githubusercontent.com/bjornsh/funktioner/main/func_download_data_from_github.R", encoding="UTF-8"))


##################################################################################
### Functions
##################################################################################

### Download and load kommun shapefile
load_sf_kommun <- function(){
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file("https://github.com/bjornsh/gis_data/raw/main/ak_riks.zip", tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.shp$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# kommun <- load_sf_kommun()


### Download and load tätort shapefile
load_sf_tatort <- function(){
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file("https://github.com/bjornsh/gis_data/raw/main/mb_riks.zip", tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.shp$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# tatort <- load_sf_tatort()


### Download and load Deso shapefile
load_sf_deso <- function(){
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file("https://github.com/bjornsh/gis_data/raw/main/deso_2018_v2.zip", tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.gpkg$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# deso <- load_sf_deso()




###  webscraping funktion from https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2
### used in download functions below
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data_frame(link = link_, url = url_))
}


### Download and load DeSO geodatabase from SCB, ie latest data

load_sf_deso2 <- function(){
  
  # create temp directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  
  # find url on download website
  links = scraplinks("https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/deso--demografiska-statistikomraden/")
  
  # download into the placeholder file
  download.file(paste0("https://www.scb.se",
                       links[links$link == "Geopackage (zip fil)",][[2]]), 
                tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.gpkg$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# deso <- load_sf_deso2()




### Download and load Regso shapefile from SCB

load_sf_regso <- function(){
  
  # create temp directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  
  # find url on download website
  links = scraplinks("https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/regso/")
  
  # download into the placeholder file
  download.file(paste0("https://www.scb.se",
                       links[links$link == "Geopackage (zip fil)",][[2]]), 
                tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.gpkg$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# regos <- load_sf_regso()


### Download and load tätort shapefile from SCB
# File size = 175MB
  
load_sf_tatort <- function(){
    
    # create temp directory
    td = tempdir()
    # create the placeholder file
    tf = tempfile(tmpdir=td, fileext=".zip")
    
    # find url on download website
    links = scraplinks("https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/tatorter/")
    
    # download into the placeholder file
    download.file(paste0("https://www.scb.se",
                         links[links$link == "Geopackage (zip-fil)",][[2]]), 
                  tf)
    
    # get name of the file ending with .shp
    fname = unzip(tf, list=TRUE)$Name[grep("\\.gpkg$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
    
    # unzip all files to the temporary directory
    unzip(tf, exdir=td, overwrite=TRUE)
    
    # identify full path to extracted file ending with .shp
    fpath = file.path(td, fname)
    
    # load the shapefile into R
    sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
  }

# tatort <- load_sf_tatort()



### Download and load småorter shapefile from SCB
# Statistiska småorter är koncentrerad bebyggelse med 50 till 199 invånare
# File size = 70MB

load_sf_smaorter <- function(){
  
  # create temp directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  
  # find url on download website
  links = scraplinks("https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/smaorter/")
  
  # download into the placeholder file
  download.file(paste0("https://www.scb.se",
                       links[links$link == "Geopackage (zip-fil)",][[2]]), 
                tf)
  
  # get name of the file ending with .shp
  fname = unzip(tf, list=TRUE)$Name[grep("\\.gpkg$", unzip(tf, list=TRUE)$Name, ignore.case = T)]
  
  # unzip all files to the temporary directory
  unzip(tf, exdir=td, overwrite=TRUE)
  
  # identify full path to extracted file ending with .shp
  fpath = file.path(td, fname)
  
  # load the shapefile into R
  sf::st_read(fpath, options = "ENCODING=WINDOWS-1252")
}






### Download SCB Län och kommuner i kodnummerordning från scb.se

load_scb_koder <- function(){
    
    # find url on download website
    links = scraplinks("https://www.scb.se/hitta-statistik/regional-statistik-och-kartor/regionala-indelningar/lan-och-kommuner/lan-och-kommuner-i-kodnummerordning/")
    
    httr::GET(paste0("https://www.scb.se",
               links[endsWith(links$url, "xls") & !is.na(links$url),][[2]]), 
        httr::write_disk(tf <- tempfile(fileext = ".xls")))
    
    
    # download into the placeholder file
    readxl::read_excel(tf, skip = 5)
    }

# scb_kod <- load_scb_koder()

### Download SCB Län och kommuner i kodnummerordning från Github (backup)

load_scb_koder2 <- function(){
GET("https://github.com/bjornsh/gis_data/raw/main/scb_kod.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf, skip = 5)
}  
  
