#' Validate DOIs
#'
#' This function validates a vector of DOIs (Digital Object Identifiers) using a regular expression pattern.#'
#' It is taken from https://github.com/libscie/retractcheck/blob/23f1e5c7d572d9470583288d951d1bad98392f82/R/utils.R#L16

#' @param dois A vector of DOIs to be validated.
#'
#' @return A named logical vector indicating whether each DOI is valid or not, names are the dois.
#'
#' @details The function uses a regular expression pattern to validate the format of each DOI in the input vector.
#' The regular expression pattern is based on the pattern used by the \code{retractcheck} package and can be found at
#' https://github.com/libscie/retractcheck/blob/23f1e5c7d572d9470583288d951d1bad98392f82/R/utils.R#L16.
#' Alternatively, you can uncomment the second regular expression pattern and comment out the first one to use the pattern
#' from the \code{rorcid} package, which can be found at https://github.com/ropensci-archive/rorcid/blob/master/R/check_dois.R.
#'
#' @examples
#' dois <- c("sbcd1234", "10.1234/abcd", "10.1002/jcb.23190", "10.47366/sabia.v5n1a3")
#' doi_valid(dois)
#'
#' @export
doi_valid <- function(dois) {
    ## regex <- "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$" # https://github.com/libscie/retractcheck/blob/23f1e5c7d572d9470583288d951d1bad98392f82/R/utils.R#L16
    regex <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'<>])\\S)+)\\b" # https://github.com/ropensci-archive/rorcid/blob/master/R/check_dois.R
    result <- grepl(
        x = dois, pattern = regex,
        perl = TRUE, ignore.case = TRUE
    )
    names(result) <- dois
    return(result)
}
