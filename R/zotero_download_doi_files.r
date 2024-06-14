#' Downloads DOI files from Zotero
#'
#' This function downloads DOI files from Zotero using the provided DOI.
#'
#' @param doi The DOI (Digital Object Identifier) of the file to be downloaded.
#' @param dest_dir The directory where the files will be downloaded.
#' 
#' @examples
#' \donotrun{
#' zotero_download_doi_files("10.1234/abcd")
#' }
zotero_download_doi_files <- function(
  doi = NULL,
  dest_dir = NULL
  ) {
  # # Load required packages
  # library(httr)
  # library(jsonlite)

  # # Define the DOI
  # doi <- "10.5281/zenodo.11370840"

  base_url <- "https://zenodo.org/api/records/"
  # Replace the DOI in the Zenodo API URL
  url <- paste0(base_url, gsub("/", "-", doi))

  # Get the record data from the Zenodo API
  record <- url |>
    httr::GET() |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  # Get the list of files
  files <- record$files

  # Download all files
  for (file in files) {
    download.file(file$url, destfile = basename(file$key))
  }
}