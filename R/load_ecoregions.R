load_ecoregions <- function() {
  
  gpkg_file <- load_private_gpkg(
    repo = "marleeannlyle1-blip/cbg-dynamic-maps",
    path = "data/ecoregions_simplified_valid.gpkg"
  )
  
  eco <- sf::st_read(
    gpkg_file,
    quiet = TRUE
  )
  
  eco <- sf::st_make_valid(eco)
  
  # Remove only geometry collections
  eco <- eco[
    sf::st_geometry_type(eco) != "GEOMETRYCOLLECTION",
  ]
  
  eco <- eco |>
    dplyr::transmute(
      eco_name = ECO_NAME,
      eco_type = BIOME_NAME
    )
  
  eco
  
}