##################################################################################
### SCB Statistikdatabas
##################################################################################

# Functions for downloading updated .csv files from 
# SCB's statistikdatabas: https://www.statistikdatabasen.scb.se
# for the whole of Sweden. Create columns for admin units 
# to simplify filtration




### population per gender, region and year
scb_pop_region_gender <- function() {
  readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131862.csv",
                    ",", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE) %>%
    tidyr::pivot_longer(!1:2, names_to = "year", values_to = "befolkning") %>%
    dplyr::mutate(lan_kod = substr(region, 1, 2),
                  lan_namn = substr(region, 4, nchar(region))) %>%
    dplyr::rename(kon = kön) %>%
    dplyr::select(lan_kod, lan_namn, year, kon, befolkning)
}

# dat <- scb_pop_region_gender()


### population per gender, kommun and year
scb_pop_kommun_gender <- function() {
  readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131907.csv",
                    ",", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE) %>%
    select(-2) %>% 
    tidyr::pivot_longer(!1:2, names_to = "year", values_to = "befolkning") %>%
    dplyr::mutate(lan_kod = substr(region, 1, 2),
                  kommun_kod = substr(region, 1, 4),
                  kommun_namn = substr(region, 6, nchar(region))) %>%
    dplyr::rename(kon = kön) %>%
    dplyr::select(lan_kod, kommun_kod, kommun_namn, year, kon, befolkning)
}

# dat <- scb_pop_kommun_gender()


### population per age class (1-year), gender, region and year
scb_pop_region_gender_age <- function() { 
  readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131903.csv",
                    ",", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE) %>%
    tidyr::pivot_longer(!1:3, names_to = "year", values_to = "befolkning") %>%
    dplyr::mutate(lan_kod = substr(region, 1, 2),
                  lan_namn = substr(region, 4, nchar(region))) %>%
    dplyr::rename(kon = kön,
                  age = 'ålder') %>%
    dplyr::select(lan_kod, lan_namn, year, kon, age, befolkning)
  }

# dat <- scb_pop_region_gender_age()





### Antal personer per medborgarskap och DeSO
scb_medborgarskap_deso <- function(lan_code = "03") {
  readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131914.csv",
                    ",", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE) %>%
    dplyr::select(-3) %>%
    dplyr::rename(deso = 1, befolkning = 3) %>% 
    mutate(lan_kod = substr(deso, 1, 2)) %>% 
    filter(lan_kod == {{ lan_code }}) %>% 
    select(-lan_kod)
}

# dat <- scb_medborgarskap_deso("01")


# scb_medborgarskap_deso <- function(lan_code) {
#   readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131914.csv",
#                     ",", escape_double = FALSE,
#                     locale = locale(encoding = "ISO-8859-1"),
#                     trim_ws = TRUE) %>%
#     dplyr::select(-3) %>%
#     dplyr::rename(deso = 1, befolkning = 3) %>% 
#     mutate(lan_kod = substr(deso, 1, 2)) %>% 
#     {if(!missing({{ lan_code }})) 
#       filter(lan_kod == {{ lan_code }}) else 
#         as.data.frame(.)} #%>% select(-lan_kod)
#   }
# 
# scb_medborgarskap_deso()




### Sammanräknad förvärvsinkomst (median, medel) per DeSO
scb_inkomst_deso <- function(lan_code = "03") {
  readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131920.csv",
                    ",", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE) %>%
    dplyr::select(-2, -3) %>%
    dplyr::rename(deso = 1, median_tkr = 2, medel_tkr = 3) %>% 
    mutate(lan_kod = substr(deso, 1, 2)) %>% 
    filter(lan_kod == {{ lan_code }}) %>% 
    select(-lan_kod)
}

# dat <- scb_inkomst_deso("01")





### Befolkning per kön och tätort och år (5-års redovisning)
scb_pop_tatort_gender <- function() {

readr::read_delim("https://www.statistikdatabasen.scb.se/sq/131947.csv",
                  ",", escape_double = FALSE,
                  locale = locale(encoding = "ISO-8859-1"),
                  trim_ws = TRUE) %>%
    dplyr::select(-3, -4) %>%
    dplyr::rename(tatort = 1, kon = 2) %>%
    tidyr::pivot_longer(!1:2, names_to = "year", values_to = "befolkning") %>%
    mutate(befolkning = gsub("\\..", NA, befolkning)) %>% 
    mutate(tatort_kod = substr(tatort, 1, 9),
           lan_kod = substr(tatort, 1, 2),
           kommun_kod = substr(tatort, 1, 4))
  }

# dat <- scb_pop_tatort_gender()
