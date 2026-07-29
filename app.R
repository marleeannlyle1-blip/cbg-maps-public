source("R/map_builder.R")

current_map <- reactive({

  build_map(
    spatial_points = spatial_points(),
    ecoregions_all = ecoregions_all,
    world = world_data(),
    eco_mode = input$eco_mode,
    selected_eco = selected_eco(),
    point_size = input$point_size,
    lot_view = input$lot_view,
    eco_pal = eco_pal()
  )

})

output$map <- renderLeaflet({
  current_map()
})
