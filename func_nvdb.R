##################################################################################
### Syfte
##################################################################################

### Bra att ha funktioner för hantering av 
### Trafikverkets Nationella vägdatabas (NVDB)




####################
### Längden av gång- och cykelvägar per kommun
####################

### Ladda "gcm_vagtyp" lager från länsdatabasen
# filer = st_layers(paste0(folder_input, list.files(folder_input, patter = "gpkg")))[1] %>% 
#   as.vector() %>% 
#   as.data.frame() %>% 
#   filter(!grepl("domainmap", name),
#          !grepl("fieldaliases", name))
# 
# gcm_vagtyp = st_read(paste0(folder_input, list.files(folder_input, patter = "gpkg")),
#                      layer = pull(filter(filer, grepl("gcm_vagtyp",name, ignore.case = TRUE))))

funk_gc_km_per_kommun = function(gcm_vagtyp_sf, kommun_sf){ 
  # gcm_vagtyp_sf = namn av gcm_vagtyp SF objekt (lager i NVDB länsdatabas)
  # kommun_sf = namn av kommun SF objekt
  gcm_vagtyp_sf %>% 
    # selektera relevanta segment
    filter(GCMTYP %in% c("Cykelfält", "Gång- och cykelbana")) %>%
    # skapa temp SF
    {. ->> urval } %>%
    # lägg till kommun data för varje segment
    st_join(., kommun_sf) %>%
    # lägg till längden för varje segment
    bind_cols(., st_length(urval)) %>%
    # ändra namn av längd kolumnen
    rename(length = length(.) - 1) %>%
    # aggregera data
    as.data.frame() %>%
    group_by(Kommun = KOMMUNNAMN) %>%
    summarise(gc_km = as.numeric(round(sum(length) / 1000, 0)))
}


# funk_gc_km_per_kommun(gcm_vagtyp, kommun)