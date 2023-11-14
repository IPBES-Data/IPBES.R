#' Abbreviate Authors
#'
#' This function abbreviates the author names in a given data frame of works.
#' The output is a list of author names in the format "First Author et al. (Year)" or "Author1 & Author2 (Year)".
#'
#' @param oa_works_df A data frame containing the works. It should have columns 'publication_year' and 'author'.
#' 'author' should be a data frame with a column 'au_display_name'. One example is the data frame from `openalexR`.
#'
#' @return A vector of abbreviated author names and publication years.
#'
#' @importFrom utils tail
#'
#' @export
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' abbreviate_authors(oa_works_df)
#' }
#'
abbreviate_authors <- function(
    oa_works_df) {
    auths <- NULL
    for (i in seq(length.out = nrow(oa_works_df))) {
        year <- oa_works_df$publication_year[[i]]
        a <- oa_works_df$author[[i]]
        if (!inherits(a, "data.frame")) {
            auth <- "NA"
            auths <- c(
                auths,
                paste0(auth, " (", year, ")")
            )
        } else {
            auth <- a$au_display_name |>
                strsplit(split = " ") |>
                sapply(
                    FUN = tail,
                    n = 1
                )
            if (length(auth) > 2) {
                auth <- paste0(auth[1], " et al.")
                auths <- c(
                    auths,
                    paste0(paste0(auth, collapse = " "), " (", year, ")")
                )
            } else {
                auths <- c(
                    auths,
                    paste0(paste0(auth, collapse = " & "), " (", year, ")")
                )
            }
        }
    }

    return(auths)
}
