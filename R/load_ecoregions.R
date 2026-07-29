load_ecoregions <- function() {

  eco <- load_private_gpkg(
    repo = "cbg-dynamic-maps",
    path = "data/ecoregions_simplified.gpkg"
  )

  if (st_crs(eco)$epsg != 4326) {
    eco <- st_transform(eco, 4326)
  }

  eco

}
