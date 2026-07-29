# Dependencies:
# sf
# dplyr
# leaflet

build_map <- function(
  ecoregions_all,
  world,
  spatial_points,
  eco_mode,
  selected_eco,
  point_size
) {

# Convert to sf
    pts_sf <- sf::st_as_sf(
      spatial_points(),
      coords = c("Lot_longitude", "Lot_latitude"),
      crs = 4326
    )
    
    # Join to ecoregions
    pts_join <- sf::st_join(
      pts_sf,
      ecoregions_all
    )
    
    # Count lots per ecoregion
    eco_counts <- dplyr::summarise %>%
      st_drop_geometry() %>%
      group_by(eco_name) %>%
      summarise(
        lot_count = n(),
        .groups = "drop"
      )
    
    # Join counts back to points
    pts_join <- left_join(
      pts_join,
      eco_counts,
      by = "eco_name"
    )
    
    # Popup text
    pts_join$popup_text <- paste0(
      "<div style='font-family:Arial;'>",
      "<strong>Ecoregion:</strong> ", pts_join$eco_name, "<br>",
      "<strong>Biome:</strong> ", pts_join$eco_type, "<br>",
      "<strong>Lots in ecoregion:</strong> ", pts_join$lot_count,
      "</div>"
    )
    
    world <- world_data()
    
    pal_countries <- colorNumeric(
      "YlGnBu",
      world$n_lots
    )
    
    m <- leaflet::leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = FALSE
      )
    ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Topographic"
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      ) %>%
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      )
    
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
    
    fill_vals <- if (input$eco_mode == "Biome") {
      eco_data$eco_type
    } else {
      eco_data$eco_name
    }
    
    # Ecoregions
    m <- m %>%
      addPolygons(
        data = eco_data,
        fillColor = eco_pal()(fill_vals),
        
        fillOpacity = ifelse(
          is.null(selected_eco()) |
            (input$eco_mode == "Biome" &
               eco_data$eco_type == selected_eco()) |
            (input$eco_mode == "Ecoregion" &
               eco_data$eco_name == selected_eco()),
          0.6,
          0.08
        ),
        
        color = "black",
        weight = 0.3,
        group = "Ecoregions",
        
        label = ~paste0(
          "<strong>",
          eco_name,
          "</strong><br>",
          eco_type
        ),
        
        highlightOptions = highlightOptions(
          weight = 2,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )
    
    # Lots
    if (input$lot_view == "Clusters") {
      
      m <- m %>%
        addCircleMarkers(
          data = pts_join,
          lng = ~st_coordinates(geometry)[,1],
          lat = ~st_coordinates(geometry)[,2],
          radius = input$point_size,
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
          lng = ~st_coordinates(geometry)[,1],
          lat = ~st_coordinates(geometry)[,2],
          radius = 2.5,
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
    
    m <- m %>%
      addLayersControl(
        baseGroups = c(
          "Topographic",
          "Satellite"
        ),
        overlayGroups = c(
          #"Lot Clusters",
          #"Individual Lots",
          "Ecoregions",
          "Countries"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      #showGroup("Lot Clusters") %>%
      #hideGroup("Individual Lots") %>%
      showGroup("Ecoregions") %>%
      hideGroup("Countries")
    
    m
  }
