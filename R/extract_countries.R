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
#' @param return_all A logical value indicating whether to return all authors' institution country codes.
#'   The order correponds to the initial vector. Missing countries are `NA`. Default is FALSE.
#'
#' @return A `data.frame with two columns:
#'    - `cc`: the country code
#'    - `weight`: the weight of the country code, i.e. `1/number of authors`. 1 if `first_author` is `TRUE``.
#'
#' @export
#'
#' @examples
#' authors <- data.frame(
#'     institution_country_code = c("US", "UK", "DE"),
#'     stringsAsFactors = FALSE
#' )
#' extract_countries(authors)
#'
#' @md

extract_countries <- function(
    authors,
    first_only = TRUE,
    return_all = FALSE) {
    countries <- lapply(
        authors,
        function(au) {
            if (length(au) <= 0) {
                res <- data.frame(
                    cc = as.character(NA),
                    weight = as.numeric(NA)
                )
            } else if (isTRUE(is.na(au))) {
                res <- data.frame(
                    cc = as.character(NA),
                    weight = as.numeric(NA)
                )
            } else if (first_only) {
                res <- data.frame(
                    cc = cc <- au[1, "institution_country_code"],
                    weight = length(cc)
                )
            } else {
                res <- data.frame(
                    cc = cc <- au[, "institution_country_code"],
                    weight = 1 / length(cc)
                )
            }
            return(res)
        }
    ) |>
        do.call(what = rbind)

    if (!return_all) {
        return(countries[!is.na(countries$cc), ])
    } else {
        return(countries)
    }
}
