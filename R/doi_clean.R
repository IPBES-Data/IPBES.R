#' Clean DOIs
#'
#' Remove resolver URLs from DOIs, keeping the full DOI intact.
#' **WARNING: This removes everything before the first `10.` part of the URL. 
#' This will likely work, onless the URL for the resolver contains `10.` 
#' which is rather unlikely**
#'
#' @param dois Character vector of DOIs to clean.
#'
#' @return Character vector of cleaned DOIs.
#' @md
#' 
#' @examples
#' dois <- c(
#'   "https://doi.org/10.1000/xyz123",
#'   "http://dx.doi.org/10.1016/j.scitotenv.2022.123456",
#'   "https://handle.net/10.1038/nphys1170",
#'   "https://resolver.example.com/10.1002/xyz123",
#'   "https://journals.somepublisher.com/doi/10.1109/5.771073",
#'   "https://customresolver.university.edu/path/10.1177/0022167819876543",
#'   "10.1000/xyz123" # Already clean
#' )
#' clean_dois(dois)
#' # [1] "10.1000/xyz123"                "10.1016/j.scitotenv.2022.123456"
#' # [3] "10.1038/nphys1170"             "10.1002/xyz123"
#' # [5] "10.1109/5.771073"              "10.1177/0022167819876543"
#' # [7] "10.1000/xyz123"
#' @export
doi_clean <- function(dois) {
  dois <- dois |>
    gsub(
      pattern = "^.*?(10\\.\\d)",
      replacement = "\\1"
    )

  return(dois)
}
