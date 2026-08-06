library(httr)

load_private_gpkg <- function(repo, path) {
  
  token <- Sys.getenv("GITHUB_PAT")
  
  tmp <- tempfile(fileext = ".gpkg")
  
  repo_url <- paste0(
    "https://api.github.com/repos/",
    repo,
    "/contents/",
    path
  )
  
  res <- httr::GET(
    repo_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/vnd.github.raw"
    ),
    httr::write_disk(tmp, overwrite = TRUE)
  )
  
  print(httr::status_code(res))
  print(file.info(tmp)$size)
  
  tmp
}

load_private_tsv <- function(repo, path) {
  
  token <- Sys.getenv("GITHUB_PAT")
  
  api_url <- paste0(
    "https://api.github.com/repos/",
    repo,
    "/contents/",
    path
  )
  
  # Request metadata
  res <- httr::GET(
    api_url,
    httr::add_headers(
      Authorization = paste("token", token)
    )
  )
  
  httr::stop_for_status(res)
  
  metadata <- httr::content(
    res,
    as = "parsed"
  )
  
  download_url <- metadata$download_url
  
  tmp <- tempfile(fileext = ".tsv")
  
  # Download actual file
  httr::GET(
    download_url,
    httr::add_headers(
      Authorization = paste("token", token)
    ),
    httr::write_disk(
      tmp,
      overwrite = TRUE
    )
  )
  
  readr::read_tsv(
    tmp,
    show_col_types = FALSE
  )
  
}