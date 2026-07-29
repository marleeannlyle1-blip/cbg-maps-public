build_map <- function(
  ecoregions_all,
  world,
  spatial_points,
  eco_mode,
  selected_eco,
  point_size
) {

  library(sf)
  library(dplyr)
  library(leaflet)

  # Convert to sf
  pts_sf <- st_as_sf(
    spatial_points,
    coords = c("Lot_longitude", "Lot_latitude"),
    crs = 4326
  )

  # Join to ecoregions
  pts_join <- st_join(
    pts_sf,
    ecoregions_all
  )

  # Count lots
  eco_counts <- pts_join %>%
    st_drop_geometry() %>%
    group_by(eco_name) %>%
    summarise(
      lot_count = n(),
      .groups = "drop"
    )

  pts_join <- left_join(
    pts_join,
    eco_counts,
    by = "eco_name"
  )

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

  pal_countries <- colorNumeric(
    "YlGnBu",
    world$n_lots
  )

  fill_vals <- if (eco_mode == "Biome") {
    ecoregions_all$eco_type
  } else {
    ecoregions_all$eco_name
  }

  eco_pal <- if (eco_mode == "Biome") {
    colorFactor(
      "Set3",
      ecoregions_all$eco_type
    )
  } else {
    colorFactor(
      scales::hue_pal()(
        length(
          unique(ecoregions_all$eco_name)
        )
      ),
      ecoregions_all$eco_name
    )
  }

  m <- leaflet(
    options = leafletOptions(
      preferCanvas = TRUE
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
      lat = 40,
      zoom = 1
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

  # Ecoregions
  m <- m %>%
    addPolygons(
      data = ecoregions_all,

      fillColor = eco_pal(fill_vals),

      fillOpacity = ifelse(
        is.null(selected_eco) |
          (eco_mode == "Biome" &
             ecoregions_all$eco_type == selected_eco) |
          (eco_mode == "Ecoregion" &
             ecoregions_all$eco_name == selected_eco),
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
      )
    )

  # Lots
  m <- m %>%
    addCircleMarkers(
      data = pts_join,

      lng = ~st_coordinates(geometry)[,1],
      lat = ~st_coordinates(geometry)[,2],

      radius = point_size,

      fillColor = "black",
      color = "white",

      weight = 1,
      fillOpacity = 0.8,

      group = "Lots",

      popup = pts_join$popup_text
    )

  m <- m %>%
    addLayersControl(
      baseGroups = c(
        "Topographic",
        "Satellite"
      ),
      overlayGroups = c(
        "Lots",
        "Ecoregions",
        "Countries"
      ),
      options = layersControlOptions(
        collapsed = FALSE
      )
    ) %>%
    showGroup("Lots") %>%
    showGroup("Ecoregions") %>%
    hideGroup("Countries")

  return(m)

}
