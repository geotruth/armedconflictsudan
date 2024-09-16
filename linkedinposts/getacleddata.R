options(scipen = 999) #so numbers dont display as scientfic notation

# Load required libraries
library(tidyverse);
library(lubridate) %>% suppressMessages() %>% suppressWarnings();library(ggrepel); library(httr) %>% suppressMessages() %>% suppressWarnings();
library(sf); library(rworldmap); library(readxl);library(ggthemes); library(ggtext)

# acled data --------------------------------------------------------------



readxl::read_xlsx('filepaths.xlsx') %>% 
  dplyr::filter(name %in% 'working') %>% pull(2) ->working

readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx') ) %>% 
  filter(Program %in% 'acled') %>%
  pull(key)->acledtoken

readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx') ) %>% 
  filter(Program %in% 'email') %>%
  pull(key)->acledemail

country = 'Sudan'
startdate = format(lubridate::as_date(x = "1500-01-01"), "%Y-%m-%d")
enddate = format(lubridate::today() , "%Y-%m-%d")

glue::glue(
  'https://api.acleddata.com/acled/read.csv?key={acledtoken}&email={acledemail}&country={country}&event_date={startdate}|{enddate}&event_date_where=BETWEEN&limit=1000000
'
) %>%
  #head %>%
  httr::GET(.) %>%
  content(., as =  "parsed")  %>%
  mutate(
    event_type = as.factor(event_type),
    event_date = as.Date(event_date),
    location = as.factor(location)
  )-> acled 
  # acled_data %>% saveRDS('acled_data.RDS')
  # read_rds("acled_data.RDS")->acled_data
  

# functions ---------------------------------------------------------------

named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}


# CRS: --------------------------------------------------------------------

useCRS <- "EPSG:4326"
CRSsp <- "+proj=longlat +datum=WGS84"

# DATA: geographic --------------------------------------------------------



rworldmap::getMap() %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(continent %in% "Africa")->africabound

sf::st_crs(africabound)->crs

rworldmap::getMap() %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(NAME %in% "Sudan") %>% 
  sf::st_transform(crs)->sudanbound

# Load administrative boundaries for Sudan
sf::read_sf("/Users/rashaelnimeiry/OneDrive - Johns Hopkins University/Arc GIS R Files/Malaria Sudan/sdn_admbndna_adm1_imwg_11302015/sdn_admbndna_adm1_imwg_11302015.shp") %>%
  mutate(admin1 = admin1Name) %>% 
  st_transform(st_crs(CRSsp)) -> sudan_boundaries 

sudan_boundaries %>% 
  st_as_sf( as(., "Spatial")) %>% 
  st_transform(st_crs(useCRS))->boundarypolygon






# spatial join: points to polygon -----------------------------------------


# as points ---------------------------------------------------------------





st_join(x= acled %>%   
          mutate(long = longitude,                 
                 lat = latitude ) %>% 
          st_as_sf(coords = c("long", "lat")) %>%
          st_set_crs(CRSsp) %>%
          st_transform(useCRS), y = boundarypolygon, join= st_within)  %>% 
  st_transform(st_crs(CRSsp)) %>% 
  mutate(admin1Name = as_factor(admin1Name),
         event_date = as.Date(event_date),
         actor = as.factor(actor1))->events #(!!)#remove duplicated columns



