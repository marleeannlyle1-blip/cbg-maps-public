source("R/map_builder.R")

current_map <- reactive({

  build_map(
    ecoregions_all = ecoregions_all,
    world = world_data(),
    spatial_points = spatial_points(),
    eco_mode = input$eco_mode,
    selected_eco = selected_eco(),
    point_size = input$point_size
  )

})

output$map <- renderLeaflet({
  current_map()
})
