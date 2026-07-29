library(shiny)

source("R/map_builder.R")
source("R/export_map.R")
source("R/github_data.R")
source("R/load_ecoregions.R")
source("R/helpers.R")
source("R/legends.R")
source("R/observers.R")

ecoregions_all <- load_ecoregions()

ui <- fluidPage(
  ...
)

server <- function(input, output, session){

  selected_eco <- reactiveVal(NULL)

  register_observers(
    input = input,
    session = session,
    selected_eco = selected_eco,
    ecoregions_all = ecoregions_all
  )

  # data()
  # filtered()
  # spatial_points()
  # world_data()
  # eco_pal()

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

  output$legend_ui <- renderUI({

    pal <- eco_pal()

    values <- if (input$eco_mode == "Biome") {
      sort(unique(ecoregions_all$eco_type))
    } else {
      sort(unique(ecoregions_all$eco_name))[1:847]
    }

    colors <- pal(values)

    build_legend(
      values = values,
      colors = colors,
      selected_value = selected_eco()
    )

  })

  output$export_map <- downloadHandler(

    filename = function() {
      paste0(
        "CBG_Map_",
        Sys.Date(),
        ".png"
      )
    },

    content = function(file) {

      export_widget <- create_export_widget(
        current_map()
      )

      export_map_png(
        map_widget = export_widget,
        file = file,
        vwidth = 1280,
        vheight = 900,
        zoom = 2
      )

    }

  )

}

shinyApp(ui, server)
