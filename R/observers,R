register_observers <- function(
    input,
    session,
    selected_eco,
    ecoregions_all
){

  # ---------------------------
  # Legend click
  # ---------------------------
  observeEvent(input$legend_click, {

    selected_eco(input$legend_click)

  })



  # ---------------------------
  # Clear filter
  # ---------------------------
  observeEvent(input$clear_filter, {

    selected_eco(NULL)

    leafletProxy("map") %>%
      clearGroup("legend_highlight") %>%
      fitBounds(
        lng1 = -180,
        lat1 = -90,
        lng2 = 180,
        lat2 = 90
      )

  })



  # ---------------------------
  # Auto-scroll legend
  # ---------------------------
  observeEvent(selected_eco(), {

    req(selected_eco())

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
  # Click lot marker
  # ---------------------------
  observeEvent(input$map_marker_click, {

    click <- input$map_marker_click

    clicked_pt <- st_sfc(
      st_point(
        c(
          click$lng,
          click$lat
        )
      ),
      crs = 4326
    )

    clicked_eco <- st_join(
      st_sf(
        geometry = clicked_pt
      ),
      ecoregions_all,
      left = FALSE
    )

    if (nrow(clicked_eco) > 0) {

      eco_match <- ecoregions_all %>%
        filter(
          eco_name ==
            clicked_eco$eco_name
        )

      leafletProxy("map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = eco_match,
          fillOpacity = 0,
          color = "#FFD700",
          weight = 2.5,
          group = "highlight"
        )

    }

  })



  # ---------------------------
  # Zoom to selected ecoregion
  # ---------------------------
  observeEvent(selected_eco(), {

    req(selected_eco())

    eco_selected <- ecoregions_all %>%
      filter(
        eco_name ==
          selected_eco()
      )

    req(nrow(eco_selected) > 0)

    bbox <- st_bbox(
      eco_selected
    )

    leafletProxy("map") %>%
      fitBounds(
        bbox["xmin"],
        bbox["ymin"],
        bbox["xmax"],
        bbox["ymax"],
        options = list(
          padding = c(10, 10),
          maxZoom = 8,
          animate = TRUE,
          duration = 1.2
        )
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

}
