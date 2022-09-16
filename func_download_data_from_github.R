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
funk_load_kommun <- function(){
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
  st_read(fpath, options = "ENCODING=WINDOWS-1252")
}

# kommun <- funk_load_kommun()


### Download and load tätort shapefile
funk_load_tatort <- function(){
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
  st_read(fpath, options = "ENCODING=WINDOWS-1252")
}







