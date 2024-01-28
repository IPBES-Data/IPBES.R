#' Check if DOIs are not retracted
#'
#' This function checks if a given list of DOIs (Digital Object Identifiers) are retracted.
#' It uses the Crossref API to query the Retraction Watch database.
#'
#' @param dois A character vector of DOIs to be checked.
#' @param cache_file A file name of the cache to be used, i.e. the downloaded retraction data. THe file is an `rds` file as downloaded from the retractionwatch site. If NULL, the data will not be cached.
#' @param email An optional email address to be included in the API request [RECOMMENDET!].
#'
#' @return A named logical vector indicating whether each DOI is retracted (`FALSE`) or or not (`TRUE`), names are the dois.
#'
#' @importFrom utils download.file read.csv
#' @examples
#' # Check if a single DOI is retracted
#' doi_not_retracted("10.1234/abcd")
#'
#' # Check if multiple DOIs are retracted
#' dois <- c("sbcd1234", "10.1234/abcd", "10.1002/jcb.23190", "10.47366/sabia.v5n1a3")
#' doi_not_retracted(dois)
#'
#' @export
doi_not_retracted <- function(dois, cache_file = NULL, email = NULL) {
    if (is.null(cache_file)) {
        tmpfile <- tempfile(fileext = ".csv")
        utils::download.file(
            url = paste0("https://api.labs.crossref.org/data/retractionwatch?", email),
            destfile = tmpfile
        )
        cache <- read.csv(tmpfile, stringsAsFactors = FALSE)
        unlink(tmpfile)
    } else {
        if (file.exists(cache_file)) {
            cache <- readRDS(cache_file)
        } else {
            tmpfile <- tempfile(fileext = ".csv")
            utils::download.file(
                url = paste0("https://api.labs.crossref.org/data/retractionwatch?", email),
                destfile = tmpfile
            )
            cache <- read.csv(tmpfile, stringsAsFactors = FALSE)
            saveRDS(cache, cache_file)
            unlink(tmpfile)
        }
    }

    result <- !(dois %in% cache$OriginalPaperDOI)
    names(result) <- dois
    return(result)
}
