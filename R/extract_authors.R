#' Extract authors' institution country codes
#'
#' This function extracts the author data which is nested in the provided `data` argument.
#'
#' The `extract_authors` function takes a data frame of with one column named `author` containing the auther information
#' nd returns these as a `tibble` with additional info of the OpenAlex `id` of the work and the `publication_y`.
#' @param data A data frame containing authors' information, including the institution country codes.
#' @param positions A character vector specifying the positions of the authors to extract.
#'   This has to be one or more of `first`, `middle`, `last` or `NA`. If logical `TRUE` all authors are extracted.
#'   Default: c("first", "middle", "last", NA). i.e. all authors.
#'
#' @return A `tibble` with all columns in the nested `data.frame` in `data$author[[i]]` plustwo columns:
#'    - `works_id`: the work id
#'    - `publication_year`: the publiction year of the work
#'
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' authors <- data.frame(
#'     institution_country_code = c("US", "UK", "DE"),
#'     stringsAsFactors = FALSE
#' )
#' extract_countries(authors)
#' }
#' @md

extract_authors <- function(
    data,
    positions = c("first", "middle", "last", NA),
    unserialize = NULL) {
    #

    if (is.null(unserialize)) {
        unserialize <- is.character(data$author[[1]])
    }

    if (unserialize) {
        data$author <- unserialize_arrow(data$author)
    }

    authors <- lapply(
        1:nrow(data),
        function(i) {
            if (!inherits(data$author[[i]], "data.frame")) {
                return(NULL)
            } else if (isTRUE(nrow(data$author[[i]]) == 0)) {
                return(NULL)
            } else {
                return(
                    cbind(
                        work_id = data$id[[i]],
                        publication_year = data$publication_year[[i]],
                        data$author[[i]]
                    )
                )
            }
        }
    ) |>
        do.call(what = rbind) |>
        tibble::as_tibble()
    #
    if (isTRUE(positions)) {
        i <- TRUE
    } else {
        i <- authors$author_position %in% positions
    }

    return(authors[i, ])
}
