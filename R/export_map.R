create_export_widget <- function(
    current_map,
    cbg_logo = "CBG_logo.png",
    uog_logo = "UoG_logo.png"
) {

  htmlwidgets::prependContent(

    current_map,

    tags$div(
      style = "
        display:flex;
        justify-content:space-between;
        align-items:center;
        background:white;
        padding:10px;
        border-bottom:1px solid #ccc;
      ",

      tags$img(
        src = cbg_logo,
        height = "60px"
      ),

      tags$div(
        style = "text-align:center;",

        tags$h1(
          "Centre for Biodiversity Genomics",
          style = "
            margin:0;
            font-family:'Segoe UI', Arial, sans-serif;
            font-weight:600;
          "
        ),

        tags$h4(
          "Global Lot Sampling Dashboard",
          style = "
            margin:0;
            color:#666;
            font-family:'Segoe UI', Arial, sans-serif;
            font-weight:400;
          "
        )
      ),

      tags$img(
        src = uog_logo,
        height = "60px"
      )
    )
  )
}

export_map_png <- function(
    map_widget,
    file,
    vwidth = 1280,
    vheight = 900,
    zoom = 2
) {

  tmp_html <- tempfile(fileext = ".html")

  htmlwidgets::saveWidget(
    map_widget,
    tmp_html,
    selfcontained = TRUE
  )

  webshot2::webshot(
    tmp_html,
    file = file,
    vwidth = vwidth,
    vheight = vheight,
    zoom = zoom,
    cliprect = "viewport"
  )
}
