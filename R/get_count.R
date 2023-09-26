#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param search_term PARAM_DESCRIPTION
#' @param dois PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openalexR]{oa_query}}, \code{\link[openalexR]{oa_request}}
#'  \code{\link[dplyr]{select}}
#' @rdname get_count
#' @export 
#' @importFrom openalexR oa_query oa_request
#' @importFrom dplyr select
get_count <- function(
    search_term,
    dois = NULL) {
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
