safe_legend_id <- function(x) {

  paste0(
    "legend_item_",
    gsub(
      "[^a-zA-Z0-9]",
      "_",
      x
    )
  )

}
build_lot_popup <- function(
    eco_name,
    eco_type,
    lot_count
){

  paste0(
    "<div style='font-family:Arial;'>",
    "<strong>Ecoregion:</strong> ",
    eco_name,
    "<br>",
    "<strong>Biome:</strong> ",
    eco_type,
    "<br>",
    "<strong>Lots in ecoregion:</strong> ",
    lot_count,
    "</div>"
  )

}
build_eco_label <- function(
    eco_name,
    eco_type
){

  paste0(
    "<strong>",
    eco_name,
    "</strong><br>",
    eco_type
  )

}
get_selected_ecoregion <- function(
    ecoregions,
    eco_mode,
    selected_value
){

  if (eco_mode == "Biome") {

    ecoregions %>%
      filter(
        eco_type == selected_value
      )

  } else {

    ecoregions %>%
      filter(
        eco_name == selected_value
      )

  }

}
zoom_to_bbox <- function(
    proxy,
    bbox
){

  proxy %>%
    fitBounds(
      bbox["xmin"],
      bbox["ymin"],
      bbox["xmax"],
      bbox["ymax"],
      options = list(
        padding = c(10,10),
        maxZoom = 8,
        animate = TRUE,
        duration = 1.2
      )
    )

}
highlight_ecoregion <- function(
    proxy,
    eco_sf,
    group_name = "highlight"
){

  proxy %>%
    clearGroup(group_name) %>%
    addPolygons(
      data = eco_sf,
      fillOpacity = 0,
      color = "#FFD700",
      weight = 3,
      group = group_name
    )

}
