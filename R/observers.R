register_observers <- function(
    input,
    session,
    selected_eco,
    selected_project,
    ecoregions_all,
    filtered
){
  
  # ---------------------------
  # Ecoregion legend click
  # ---------------------------
  observeEvent(input$legend_click, {
    
    selected_eco(
      input$legend_click
    )
    
  })
  
  
  # ---------------------------
  # Clear ecoregion filter
  # ---------------------------
  observeEvent(input$clear_filter, {
    
    selected_eco(NULL)
    
    leafletProxy("map") %>%
      clearGroup("legend_highlight") %>%
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      )
    
  })
  
  
  # ---------------------------
  # Auto-scroll legend
  # ---------------------------
  observeEvent(selected_eco(), {
    
    req(!is.null(selected_eco()))
    
    safe_id <- paste0(
      "legend_item_",
      gsub(
        "[^a-zA-Z0-9]",
        "_",
        selected_eco()
      )
    )
    
    session$sendCustomMessage(
      "scrollLegend",
      safe_id
    )
    
  })
  
  
  # ---------------------------
  # Zoom to selected ecoregion
  # ---------------------------
  observeEvent(selected_eco(), {
    
    req(!is.null(selected_eco()))
    
    eco_selected <- if (
      identical(
        input$eco_mode,
        "Biome"
      )
    ) {
      
      dplyr::filter(
        ecoregions_all,
        eco_type == selected_eco()
      )
      
    } else {
      
      dplyr::filter(
        ecoregions_all,
        eco_name == selected_eco()
      )
      
    }
    
    req(nrow(eco_selected) > 0)
    
    centre <- eco_selected %>%
      sf::st_union() %>%
      sf::st_transform(3857) %>%
      sf::st_centroid() %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
    
    leafletProxy("map") %>%
      setView(
        lng = centre[1],
        lat = centre[2],
        zoom = 7
      ) %>%
      clearGroup("legend_highlight") %>%
      addPolygons(
        data = eco_selected,
        fillOpacity = 0,
        color = "#FFD700",
        weight = 3,
        group = "legend_highlight"
      )
    
  })
  
  
  # ---------------------------
  # Project selection
  # ---------------------------
  observeEvent(input$project_click, {
    
    selected_project(
      input$project_click
    )
    
    #cat(
    #  "Project selected:",
    #  selected_project(),
    #  "\n"
    #)
    
  })
  
  
  # ---------------------------
  # Clear project
  # ---------------------------
  observeEvent(input$clear_project, {
    
    selected_project(NULL)
    
  })
  
  
  # ---------------------------
  # Zoom to project
  # ---------------------------
  observeEvent(selected_project(), {
    
    req(!is.null(selected_project()))
    
    project_pts <- filtered() %>%
      dplyr::filter(
        trackingsl_projectcode ==
          selected_project()
      )
    
    req(nrow(project_pts) > 0)
    
    project_sf <- sf::st_as_sf(
      project_pts,
      coords = c(
        "Lot_longitude",
        "Lot_latitude"
      ),
      crs = 4326
    )
    
    #cat(
    #  "Project:",
    #  selected_project(),
    #  "\nRows:",
    #  nrow(project_sf),
    #  "\n"
    #)
    
    if (nrow(project_sf) < 3) {
      
      bbox <- sf::st_bbox(
        project_sf
      )
      
      leafletProxy("map") %>%
        fitBounds(
          bbox["xmin"],
          bbox["ymin"],
          bbox["xmax"],
          bbox["ymax"]
        )
      
    } else {
      
      project_hull <- project_sf %>%
        sf::st_transform(3857) %>%
        sf::st_buffer(50000) %>%
        sf::st_union() %>%
        sf::st_transform(4326)
      
      centre <- project_hull %>%
        sf::st_transform(3857) %>%
        sf::st_centroid() %>%
        sf::st_transform(4326) %>%
        sf::st_coordinates()
      
      leafletProxy("map") %>%
        setView(
          lng = centre[1],
          lat = centre[2],
          zoom = 10
        )
      
    }
    
  })
  
  
  # ---------------------------
  # Populate project selector
  # ---------------------------
  observe({
    
    req(filtered())
    
    updateSelectizeInput(
      session,
      "project_click",
      choices = sort(
        unique(
          na.omit(
            filtered()$trackingsl_projectcode
          )
        )
      ),
      selected = character(0),
      server = TRUE
    )
    
  })
  
}