source(here::here("R", "transform_state.R"))
source(here::here("R", "elide_states.R"))

get_pumas <- function() {
  
  if (file.exists(here::here("shapefiles", "pumas.shp"))) {
    
    shapefiles <- sf::st_read(here::here("shapefiles", "pumas.shp")) %>%
      dplyr::mutate_if(is.factor, as.character)
    
    return(shapefiles)
    
  } else {
    
    get_shapefile <- function(fips_code) {

      shape <- tigris::pumas(state = fips_code,
                             class = "sf",
                             cb = TRUE)

      return(shape)

    }
    
    state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
                    "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
                    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
                    "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
                    "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
                    "56")
    
    shapefiles <- state_fips %>%
      purrr::map(get_shapefile) %>%
      purrr::reduce(rbind)
    
    shapefiles <- shapefiles %>%
      #mutate(STATEFP = stringr::str_sub(GEOID, 1, 2)) %>%
      dplyr::rename(STATEFP = STATEFP10) %>%
      elide_states()
    
    sf::st_write(shapefiles, here::here("shapefiles", "pumas.shp"))
    
    return(shapefiles)
    
  }
}
