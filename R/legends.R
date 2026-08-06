# ----------------------------------
# Ecoregion legend
# ----------------------------------
build_legend <- function(
    values,
    colors,
    selected_value
){
  
  tagList(
    
    actionButton(
      "clear_filter",
      "Clear",
      width = "100%"
    ),
    
    tags$input(
      id = "eco_search",
      type = "text",
      placeholder = "Search ecoregions...",
      class = "form-control",
      onkeyup = "
    var filter = this.value.toLowerCase();
    var items = document.getElementsByClassName('legend-item');

    for (var i = 0; i < items.length; i++) {

      var txt =
        items[i].innerText.toLowerCase();

      items[i].style.display =
        txt.includes(filter)
        ? ''
        : 'none';
    }
  "
    ),
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler(
        'scrollLegend',
        function(id) {

          var el =
            document.getElementById(id);

          if (el) {

            el.scrollIntoView({
              behavior:'smooth',
              block:'center'
            });

          }
        }
      );
    ")),
    
    tags$div(
      
      id = "legendItems",
      
      lapply(
        seq_along(values),
        
        function(i){
          
          item_id <- paste0(
            'legend_item_',
            gsub(
              '[^a-zA-Z0-9]',
              '_',
              values[i]
            )
          )
          
          is_selected <- isTRUE(
            !is.null(selected_value) &&
              !is.na(selected_value) &&
              values[i] == selected_value
          )
          
          tags$div(
            
            id = item_id,
            
            class = "legend-item",
            
            onclick = sprintf(
              "Shiny.setInputValue(
                'legend_click',
                '%s',
                {priority:'event'}
              );",
              values[i]
            ),
            
            style = paste0(
              
              "cursor:pointer;",
              "display:flex;",
              "align-items:center;",
              "margin:4px 0;",
              
              if (is_selected)
                "background-color:#ffffcc;font-weight:bold;"
              else
                ""
              
            ),
            
            tags$div(
              
              style = paste0(
                "width:15px;",
                "height:15px;",
                "background:",
                colors[i],
                ";margin-right:6px;",
                "border:1px solid #999;"
              )
              
            ),
            
            tags$span(values[i])
            
          )
          
        }
      )
      
    )
    
  )
  
}


# ----------------------------------
# Projects tab
# ----------------------------------
build_project_tab <- function() {
  
  tagList(
    
    actionButton(
      "clear_project",
      "Clear",
      width = "100%"
    ),
    
    selectizeInput(
      "project_click",
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        placeholder = "Search projects..."
      ),
      width = "100%"
    )
    
  )
  
}


# ----------------------------------
# Tabbed legend
# ----------------------------------
build_tabbed_legend <- function() {
  
  tags$div(
    
    id = "ecoLegend",
    
    bslib::navset_tab(
      
      bslib::nav_panel(
        "Ecoregions",
        uiOutput("ecoregion_legend_ui")
      ),
      
      bslib::nav_panel(
        "Projects",
        build_project_tab()
      )
      
    )
    
  )
  
}