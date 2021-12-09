elide_states <- function(shapefile) {
  
  # alaska ------------------------------------------------------------------
  alaska <- shapefile %>%
    filter(STATEFP == "02")
  
  alaska <- alaska %>%
    st_crop(y = c(xmin = -180,
                  ymin = 0,
                  xmax = -120,
                  ymax = 90))
  
  alaska <- as(alaska, Class = "Spatial")
  
  # hawaii ------------------------------------------------------------------
  hawaii <- shapefile %>%
    filter(STATEFP == "15") 
  
  hawaii <- hawaii %>%
    st_crop(y = c(xmin = -161,
                  ymin = 18,
                  xmax = -150,
                  ymax = 23))
  
  hawaii <- as(hawaii, Class = "Spatial")
  
  # continental us ----------------------------------------------------------
  # convert shapes to sp
  continental_us <- shapefile %>%
    filter(!STATEFP %in% c("02", "15"))
  
  continental_us <- as(continental_us, Class = "Spatial")
  
  continental_us <- sp::spTransform(continental_us, CRS("+init=epsg:2163"))
  
  exclude <- c("02", "15", "60", "66", "69", "72", "78")
  
  continental_us <- continental_us[!continental_us$STATEFP %in% exclude, ] 
  
  # transform alaska and hawaii
  alaska_test <- alaska %>%
    sp::spTransform(CRS("+init=epsg:2163"))%>%
    transform_state(-35, 2, c(-2600000, -2300000))
  
  proj4string(alaska_test) <- proj4string(continental_us)
  
  alaska_test <- alaska_test %>%
    st_as_sf() %>%
    st_transform(crs = 102003) %>%
    st_simplify()
  
  hawaii_test <- hawaii %>%
    sp::spTransform(CRS("+init=epsg:2163"))%>%
    transform_state(-35, 0.8, c(-1170000, -2363000))
  
  proj4string(hawaii_test) <- proj4string(continental_us)
  
  hawaii_test <- hawaii_test %>%
    st_as_sf() %>%
    st_transform(crs = 102003) %>%
    st_simplify()
  
  # combine -----------------------------------------------------------------
  
  continental_us <- continental_us %>%
    st_as_sf() %>%
    st_transform(crs = 102003) %>%
    st_simplify()
  
  full_us <- continental_us %>%
    rbind(alaska_test) %>%
    rbind(hawaii_test)
  
  return(full_us)
  
}