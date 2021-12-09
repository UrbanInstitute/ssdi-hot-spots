map_frequency <- function(data, 
                          puma_shapes = shapefiles,
                          state_shapes = states,
                          state_subset = NULL,
                          state_labels = FALSE,
                          var, 
                          disorder,
                          title,
                          colors = reds,
                          map_only = FALSE,
                          legend_only = FALSE) {
  
  data_temp <- data %>%
    filter(.data$dxpriadult == disorder)
  
  shapes_temp <- left_join(puma_shapes, data_temp, by = c("GEOID10" = "pumaid"))
  
  if(!is.null(state_subset)) {
    
    shapes_temp <- shapes_temp %>%
      dplyr::filter(str_sub(.data$GEOID10, 1, 2) %in% state_subset)
    
    state_shapes <- state_shapes %>%
      dplyr::filter(.data$STATEFP %in% state_subset)
    
    
  }
  
  
  legend <- data_temp %>%
    count(hotspot_frequency) %>%
    ggplot() +
    geom_col(aes(x = hotspot_frequency, y = n, fill = hotspot_frequency, color = "a")) +
    scale_color_manual(values = "black") +
    scale_x_continuous(breaks = seq(0, 35, 5),
                       limits = c(0, 15),
                       expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, NA))) +
    scale_fill_gradientn(expand = c(0,0),
                         colors = colors[-1],
                         na.value = "white",
                         labels = scales::comma) +
    labs(x = "Hot spot frequency",
         y = NULL) +
    remove_ticks() +
    theme(panel.grid = element_blank()) +
    guides(color = "none", fill = "none")
  
  temp_plot <- ggplot() +
    geom_sf(data = shapes_temp,
            aes(fill = {{var}}),
            color = "white",
            size = 0.0) +
    geom_sf(data = state_shapes,
            color = "black",
            fill = NA,
            size = 0.05) +
    scale_fill_gradientn(limits = c(0, NA), 
                         expand = c(0,0),
                         colors = colors[-1], 
                         na.value = "white",
                         labels = scales::comma) +
    theme_urbn_map() +
    theme(legend.title = element_blank()) +
    guides(fill = "none")
  
  if (state_labels) {
    
    temp_plot <- temp_plot +
      geom_sf_text(data = state_shapes, aes(label = STUSPS), size = 8)
    
    
  }
  
  if (map_only) {
  
    temp_plot <- temp_plot +
      labs(subtitle = disorder)
    
    return(temp_plot)
    
  }
  
  if (legend_only) {
    
    legend <- legend +
      labs(subtitle = disorder)
    
    return(legend)
    
  }
  
  final_plot <- urbn_plot(
    urbn_title(title),
    urbn_subtitle(disorder),
    temp_plot,
    legend,
    heights = c(0.05, 0.05, 0.6, 0.3)
  )

  #plot(final_plot)
  
  #return(final_plot)
  
}
