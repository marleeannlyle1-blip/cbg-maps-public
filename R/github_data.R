library(httr)
library(sf)

load_ecoregions <- function() {

  token <- Sys.getenv("GITHUB_PAT")

  tmp <- tempfile(fileext = ".gpkg")

  res <- GET(
    "https://api.github.com/repos/USERNAME/cbg-dynamic-maps/contents/data/ecoregions_simplified.gpkg",
    add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/vnd.github.raw"
    ),
    write_disk(tmp, overwrite = TRUE)
  )

  st_read(tmp, quiet = TRUE)

}
