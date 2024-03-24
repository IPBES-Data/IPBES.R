#' Extract authors' institution country codes
#'
#' This function extracts the institution country codes of authors from a given data frame.
#' The `extract_authors` function takes a data frame of authors' information and extracts the institution country codes.
#' By default, it returns a vector containing only the first institution country code for the first author.
#' If `first_only` is set to FALSE, it returns a vector containing the institution country codes for all authors.
#' If an author has no institution country code or if it is missing, NA is returned.
#' @param authors A data frame containing authors' information, including the institution country codes.
#' @param first_only A logical value indicating whether to extract only the first author's institution country code
#'   or for all authors. Default is TRUE.
#'
#' @return A vector of institution country codes corresponding to the authors.
#'
#' @export
#'
#' @examples
#' authors <- data.frame(
#'     institution_country_code = c("US", "UK", "DE"),
#'     stringsAsFactors = FALSE
#' )
#' extract_authors(authors)
#'
#' @md

extract_countries <- function(
    authors,
    first_only = TRUE) {
    sapply(
        authors,
        function(au) {
            if (length(au) <= 0) {
                return(NA)
            } else {
                if (first_only) {
                    res <- au[1, "institution_country_code"]
                } else {
                    res <- au[, "institution_country_code"]
                }
                ifelse(
                    is.null(res),
                    NA,
                    res
                )
            }
        }
    )
}
