map_benefits <- function(data, 
                         puma_shapes = shapefiles,
                         var, 
                         map_year,
                         title,
                         disorder,
                         border_color = "white",
                         proportion,
                         colors = reds) {
  
  di_temp <- data %>%
    filter(.data$year == map_year) %>%
    filter(.data$dxpriadult == disorder) 
  
  shapes_temp <- left_join(puma_shapes, di_temp, by = c("GEOID10" = "pumaid"))
  
  if (proportion) {
  
    temp_plot <- ggplot() +
      geom_sf(data = shapes_temp,
              aes(fill = {{var}}, color = factor(diaward_hot_q10)),
              size = 0.1) +
      scale_color_manual(values = c(NA, "#FCE205"), na.value = NA) +
      guides(color = FALSE) +
      geom_sf(data = states,
              color = border_color,
              fill = NA,
              size = 0.1) +
      scale_fill_gradientn(limits = c(0, NA), 
                           expand = c(0,0),
                           colors = colors, 
                           na.value = "white",
                           labels = scales::percent_format(accuracy = 1L)) +
      labs(title = paste0(title, " (", map_year, ")"),
           subtitle = disorder) +
      theme_urbn_map() +
      theme(legend.title = element_blank())
  
  } else {
    
    temp_plot <- ggplot() +
      geom_sf(data = shapes_temp,
              aes(fill = {{var}}),
              color = "white",
              size = 0.0) +
      geom_sf(data = states,
              color = border_color,
              fill = NA,
              size = 0.1) +
      scale_fill_gradientn(limits = c(0, NA), 
                           expand = c(0,0),
                           colors = colors, 
                           na.value = "white",
                           labels = scales::comma) +
      labs(title = paste0(title, " (", map_year, ")"),
           subtitle = disorder) +
      theme_urbn_map() +
      theme(legend.title = element_blank())
    
  }
    
  print(temp_plot)
  
}