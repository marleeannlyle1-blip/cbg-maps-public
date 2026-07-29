build_legend <- function(
    values,
    colors,
    selected_value
) {

  tags$div(
    id = "ecoLegend",

    # Clear button
    tags$button(
      "Clear",
      onclick = "Shiny.setInputValue('clear_filter', true, {priority: 'event'})",
      style = "margin-bottom:8px; width:100%;"
    ),

    # Search box
    tags$input(
      type = "text",
      placeholder = "Search...",
      onkeyup = "
        var filter = this.value.toLowerCase();
        var items = document.getElementsByClassName('legend-item');
        for (var i = 0; i < items.length; i++) {
          var txt = items[i].innerText.toLowerCase();
          items[i].style.display = txt.includes(filter) ? '' : 'none';
        }
      ",
      style = "width:100%; margin-bottom:6px;"
    ),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollLegend', function(id) {
        var el = document.getElementById(id);
        if (el) {
          el.scrollIntoView({behavior: 'smooth', block: 'center'});
        }
      });
    ")),

    tags$div(
      id = "legendItems",

      lapply(seq_along(values), function(i) {

        item_id <- paste0(
          "legend_item_",
          gsub("[^a-zA-Z0-9]", "_", values[i])
        )

        is_selected <-
          !is.null(selected_value) &&
          values[i] == selected_value

        tags$div(

          id = item_id,

          class = "legend-item",

          onclick = sprintf(
            "Shiny.setInputValue('legend_click', '%s', {priority:'event'});",
            values[i]
          ),

          style = paste0(
            "cursor:pointer;",
            "display:flex;",
            "align-items:center;",
            "margin:4px 0;",
            if (is_selected)
              "background-color:#ffffcc;font-weight:bold;"
            else ""
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

      })
    )
  )
}
