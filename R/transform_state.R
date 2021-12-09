# transform AK/HI from shapefile -----------------------------------------------
# see https://github.com/wmurphyrd/fiftystater ---------------------------------
transform_state <- function(object, rot, scale, shift) {
  object %>% 
    maptools::elide(rotate = rot) %>%
    maptools::elide(scale = max(apply(sp::bbox(object), 1, diff)) / scale) %>%
    maptools::elide(shift = shift)
}