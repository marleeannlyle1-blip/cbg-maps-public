# Dependencies:
# sf
# dplyr
# leaflet
# scales

build_map <- function(
    ecoregions_all,
    world,
    spatial_points,
    eco_mode,
    selected_eco,
    selected_project,
    point_size,
    lot_view,
    eco_pal,
    export_mode = FALSE
) {
  
  # Convert to sf
  pts_sf <- sf::st_as_sf(
    spatial_points,
    coords = c("Lot_longitude", "Lot_latitude"),
    crs = 4326
  )
  
  # Join to ecoregions
  pts_join <- sf::st_join(
    pts_sf,
    ecoregions_all
  )
  
  # Count lots per ecoregion
  eco_counts <- pts_join %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(eco_name) %>%
    dplyr::summarise(
      lot_count = dplyr::n(),
      .groups = "drop"
    )
  
  # Join counts back to points
  pts_join <- dplyr::left_join(
    pts_join,
    eco_counts,
    by = "eco_name"
  )
  
  # Popup text
  pts_join$popup_text <- paste0(
    "<div style='font-family:Arial;'>",
    "<strong>Ecoregion:</strong> ",
    pts_join$eco_name,
    "<br>",
    "<strong>Biome:</strong> ",
    pts_join$eco_type,
    "<br>",
    "<strong>Lots in ecoregion:</strong> ",
    pts_join$lot_count,
    "</div>"
  )
  
  
  pal_countries <- leaflet::colorNumeric(
    "YlGnBu",
    world$n_lots
  )
  
  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(
      preferCanvas = TRUE,
      zoomControl = FALSE
    )
  ) %>%
    addProviderTiles(
      providers$Esri.WorldTopoMap,
      group = "Topographic"
    ) %>%
    setView(
      lng = 0,
      lat = 20,
      zoom = 2
    )
  
  # Add Satellite only in app
  if (!export_mode) {
    
    m <- m %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      )
    
  }
  
  # Countries
  m <- m %>%
    addPolygons(
      data = world,
      fillColor = ~pal_countries(n_lots),
      fillOpacity = 0.7,
      color = "grey60",
      weight = 0.5,
      group = "Countries",
      label = ~admin
    )
  
  eco_data <- ecoregions_all
  
  fill_vals <- if (eco_mode == "Biome") {
    eco_data$eco_type
  } else {
    eco_data$eco_name
  }
  
  # Ecoregions
  selected_flag <- if (is.null(selected_eco)) {
    
    rep(TRUE, nrow(eco_data))
    
  } else if (eco_mode == "Biome") {
    
    !is.na(eco_data$eco_type) &
      eco_data$eco_type == selected_eco
    
  } else {
    
    !is.na(eco_data$eco_name) &
      eco_data$eco_name == selected_eco
    
  }
  
  m <- m %>%
    addPolygons(
      data = eco_data,
      fillColor = eco_pal(fill_vals),
      
      fillOpacity = ifelse(
        selected_flag,
        0.3,
        0.08
      ),
      
      color = "black",
      weight = 0.3,
      group = "Ecoregions",
      
      label = lapply(
        sprintf(
          "<strong>%s</strong><br>%s",
          eco_data$eco_name,
          eco_data$eco_type
        ),
        htmltools::HTML
      ),
      
      highlightOptions = highlightOptions(
        weight = 2,
        color = "grey",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  
  # Lots
  if (lot_view == "Clusters") {
    
    m <- m %>%
      addCircleMarkers(
        data = pts_join,
        lng = ~sf::st_coordinates(geometry)[,1],
        lat = ~sf::st_coordinates(geometry)[,2],
        radius = point_size,
        fillColor = "black",
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        clusterOptions = markerClusterOptions(),
        group = "Lots",
        popup = pts_join$popup_text
      )
    
  } else {
    
    m <- m %>%
      addCircleMarkers(
        data = pts_join,
        lng = ~sf::st_coordinates(geometry)[,1],
        lat = ~sf::st_coordinates(geometry)[,2],
        radius = point_size,
        fillColor = "black",
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        group = "Lots",
        popup = pts_join$popup_text
      )
    
  }
  
  m <- m %>%
    addScaleBar(
      position = "bottomleft"
    )
  
  if (!export_mode) {
    
    m <- m %>%
      addLayersControl(
        baseGroups = c(
          "Topographic",
          "Satellite"
        ),
        overlayGroups = c(
          "Ecoregions",
          "Countries",
          "Projects"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      showGroup("Ecoregions") %>%
      showGroup("Projects") %>%
      hideGroup("Countries")
    
  }
  
  if (!is.null(selected_project)) {
    
    #cat(
    #  "Selected project:",
    #  selected_project,
    #  "\n"
    #)
    
    project_pts <- spatial_points %>%
      dplyr::filter(
        trackingsl_projectcode ==
          selected_project
      )
    
    #cat(
    #  "Project rows:",
    #  nrow(project_pts),
    #  "\n"
    #)
    
    if (nrow(project_pts) > 0) {
      
      project_sf <- sf::st_as_sf(
        project_pts,
        coords = c(
          "Lot_longitude",
          "Lot_latitude"
        ),
        crs = 4326
      )
      
      if (nrow(project_sf) >= 3) {
        
        project_hull <- project_sf %>%
          sf::st_transform(3857) %>%
          sf::st_buffer(50000) %>%
          sf::st_union() %>%
          sf::st_transform(4326)
        
        project_hull <- sf::st_sf(
          geometry = project_hull
        )
        
        #print(sf::st_geometry_type(project_hull))
        #print(sf::st_bbox(project_hull))
        
        m <- m %>%
          addPolygons(
            data = project_hull,
            color = "#FF6600",
            weight = 5,
            dashArray = "8,4",
            fillColor = "#FF9900",
            fillOpacity = 0.15,
            group = "Projects"
          )
        
      } else {
        
        m <- m %>%
          addCircleMarkers(
            data = project_sf,
            radius = point_size * 3,
            color = "red",
            fillColor = "red",
            fillOpacity = 1,
            weight = 2,
            group = "Projects"
          )
        
      }
      
    }
    
  }
  
  return(m)
}