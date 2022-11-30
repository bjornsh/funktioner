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

