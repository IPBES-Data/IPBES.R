#' Get Count
#'
#' This function takes a search term and a list of DOIs, and returns the count of the search term in each DOI.
#' If duplicate DOIs are provided, the function will stop and throw an error.
#'
#' @param search_term The term to search for.
#' @param dois A list of DOIs to search within. Default is NULL.
#'
#' @return A named list where the names are the DOIs and the values are the counts of the search term in each DOI.
#'
#' @importFrom openalexR oa_query oa_request
#' @importFrom dplyr select
#'
#' @autoglobal
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_count("climate change", c("10.1038/nature12350", "10.1126/science.1259855"))
#' }
get_count <- function(
    search_term,
    dois = NULL,
    ...) {
    if (length(dois) != length(unique(dois))) {
        stop("\n Duplicate DOIs are not supported!")
    }
    count <- as.list(dois) |>
        sapply(
            FUN = function(doi) {
                openalexR::oa_query(search = search_term, filter = c(doi = doi)) |>
                    openalexR::oa_request(count_only = TRUE) |>
                    unlist()
            }
        ) |>
        t() |>
        as.data.frame() |>
        dplyr::select(count) |>
        unlist()
    names(count) <- dois
    return(count)
}
