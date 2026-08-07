library(shiny)
library(readr)
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(leaflet)
library(rnaturalearth)
library(bslib)
library(htmlwidgets)
library(webshot2)

source("R/map_builder.R")
source("R/export_map.R")
source("R/github_data.R")
source("R/load_ecoregions.R")
source("R/helpers.R")
source("R/legends.R")
source("R/observers.R")

ecoregions_all <- load_ecoregions()

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$style(HTML("
#ecoLegend {
  position: absolute;
  bottom: -40px;
  left: 20px;
  z-index: 1000;
  background: white;
  padding: 10px;
  border-radius: 6px;
  width: 500px;
  height: 500px;
}

html, body {
  height: 100%;
  overflow: hidden;
}

#ecoLegend .btn {
  width: 100%;
  margin-bottom: 8px;
}

#ecoLegend .form-control {
  margin-bottom: 8px;
}

#ecoLegend .selectize-control {
  margin-bottom: 8px;
}

#ecoLegend .selectize-input {
  min-height: 38px;
}

#ecoLegend .tab-content {
  height: 280px;
  overflow-y: auto;
}

#ecoLegend .nav-tabs {
  margin-bottom: 8px;
}
")),
  
  tags$div(
    style = "
    display:flex;
    align-items:center;
    justify-content:space-between;
    margin-bottom:15px;
    padding:10px;
    background:white;
  ",
    
    # Left side (CBG)
    tags$div(
      style = "
      display:flex;
      align-items:center;
      gap:15px;
    ",
      
      tags$img(
        src = "CBG_logo.png",
        height = "80px"
      ),
      
      tags$div(
        tags$h1(
          "Centre for Biodiversity Genomics",
          style = "margin:0;"
        ),
        
        tags$h4(
          "Global Lot Sampling Dashboard",
          style = "margin:0; color:#666;"
        )
      )
    ),
    
    # Right side (U of Guelph)
    tags$img(
      src = "UoG_logo.png",
      height = "70px"
    )
    ),
  
  sidebarLayout(
    sidebarPanel(
      
      #selectInput("colour_mode", "Colour lots by:",
      #choices = c("Ecoregion type", "Project")),
      
      selectInput("eco_mode", "Ecoregion colouring:",
                  choices = c("Biome", "Ecoregion"),
                  selected = "Ecoregion"),
      
      sliderInput("point_size", "Point size", 2, 8, 3),
      downloadButton(
        "export_map",
        "Export Current View"
      ),
      
      radioButtons(
        "lot_view",
        "Display lots as:",
        choices = c(
          "Clusters",
          "Individual Lots"
        ),
        selected = "Clusters"
      )
    ),
    
    mainPanel(
      leafletOutput(
        "map",
        height = "calc(100vh - 180px)"
      ),
      uiOutput("legend_ui"),   # custom legend
      br(),
      verbatimTextOutput("summary")
    )
  )
  )

#data_url <- "https://raw.githubusercontent.com/marleeannlyle1-blip/cbg-dynamic-maps/refs/heads/main/GMP_Map_Lots.tsv?token=GHSAT0AAAAAAEBAYKZUNC56A5MZ24IVVYIC2TLJL7A"

server <- function(input, output, session){

  selected_eco <- reactiveVal(NULL)
  selected_project <- reactiveVal(NULL)
  current_bounds <- reactiveVal(NULL)

  # data()
  data <- reactive({
    
    load_private_tsv(
      repo = "marleeannlyle1-blip/cbg-dynamic-maps",
      path = "GMP_Map_Lots.tsv"
    )
    
  })
  
  # filtered()
  filtered <- reactive({
    data()
  })
  
  register_observers(
    input = input,
    session = session,
    selected_eco = selected_eco,
    selected_project = selected_project,
    ecoregions_all = ecoregions_all,
    filtered = filtered
  )
  
  # spatial_points
  spatial_points <- reactive({
    
    filtered() %>%
      dplyr::filter(
        !is.na(Lot_latitude),
        !is.na(Lot_longitude)
      )
    
  })
  
  # world_data()
  world_data <- reactive({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    pts <- st_as_sf(spatial_points(), coords = c("Lot_longitude", "Lot_latitude"), crs = 4326)
    joined <- st_join(pts, world)
    
    counts <- joined %>%
      st_drop_geometry() %>%
      group_by(admin) %>%
      summarise(n_lots = n(), .groups = "drop")
    
    world <- world %>%
      left_join(counts, by = "admin")
    
    world$n_lots[is.na(world$n_lots)] <- 0
    world
  })
  
  # eco_pal()
  eco_pal <- reactive({
    if (input$eco_mode == "Biome") {
      colorFactor("Set3", ecoregions_all$eco_type)
    } else {
      colorFactor(scales::hue_pal()(length(unique(ecoregions_all$eco_name))),
                  ecoregions_all$eco_name)
    }
  })
  
  observe({
    
    if (!is.null(input$map_bounds)) {
      
      current_bounds(
        input$map_bounds
      )
      
    }
    
  })

  current_map <- reactive({
    
    build_map(
      spatial_points = spatial_points(),
      ecoregions_all = ecoregions_all,
      world = world_data(),
      eco_mode = input$eco_mode,
      selected_eco = selected_eco(),
      selected_project = selected_project(),
      point_size = input$point_size,
      lot_view = input$lot_view,
      eco_pal = eco_pal(),
      export_mode = FALSE
    )
    
  })

  output$map <- renderLeaflet({
    current_map()
  })

  output$legend_ui <- renderUI({
    
    build_tabbed_legend()
    
  })
  
  output$ecoregion_legend_ui <- renderUI({
    
    pal <- eco_pal()
    
    eco_values <- if (input$eco_mode == "Biome") {
      
      sort(
        unique(
          ecoregions_all$eco_type
        )
      )
      
    } else {
      
      sort(
        unique(
          ecoregions_all$eco_name
        )
      )
      
    }
    
    eco_colors <- pal(
      eco_values
    )
    
    build_legend(
      values = eco_values,
      colors = eco_colors,
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
      
      bounds <- current_bounds()
      
      export_widget <- create_export_widget(
        current_map()
      )
      
      if (!is.null(bounds)) {
        
        export_widget <- export_widget %>%
          fitBounds(
            bounds$west,
            bounds$south,
            bounds$east,
            bounds$north
          )
        
      }
      
      export_map_png(
        map_widget = export_widget,
        file = file,
        vwidth = 1280,
        vheight = 900,
        zoom = 2
      )
      
    }
    
  )
  
#  output$summary <- renderText({
#    paste0(
#      "Total Lots: ",
#      nrow(filtered())
#    )
#  })


}

shinyApp(ui, server)
