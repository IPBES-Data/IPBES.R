library(httr2)



#' Check if DOIs exist
#'
#' This function checks if a given list of DOIs exist by sending HTTP requests to the DOI resolver.
#'
#' @details This function uses the httr package to send HTTP GET requests to the DOI resolver and checks the response status code. A status code of 200 indicates that the DOI exists, while any other status code indicates that the DOI does not exist.
#'
#' @param dois A character vector of DOIs to check.
#' @param cache_file A file name of the cache to be used, i.e. the confirmed existing dois. The format is a character vector with the DOIs which exist. If the cache exist, it will be updated at the end.
#'
#' @return A named logical vector indicating whether each DOI does exist or not, names are the dois.
#'
#' @examples
#' dois <- c("sbcd1234", "10.1234/abcd", "10.1002/jcb.23190", "10.47366/sabia.v5n1a3")
#' doi_exists(dois)
#' # Output: [1] FALSE  TRUE
#'
#' @importFrom httr2 request req_headers req_throttle req_retry req_error req_perform resp_status
#'
#' @export
doi_exists <- function(dois, cache_file = NULL) {
    if (is.null(cache_file)) {
        dois_to_check <- 1:length(dois)
        cache <- NULL
    } else {
        if (file.exists(cache_file)) {
            cache <- readRDS(cache_file)
            dois_to_check <- (1:length(dois))[!(dois %in% cache)]
        } else {
            dois_to_check <- 1:length(dois)
            cache <- NULL
        }
    }

    total <- length(dois_to_check)
    result_dois_checked <- sapply(
        dois_to_check,
        function(i) {
            # Print progress
            cat(sprintf("\rProgress: %d of %d", i, total))

            if (is.na(dois[i])) {
                return(NA)
            } else {
                tryCatch(
                    {
                        status <- httr2::request(paste0("https://doi.org/", dois[i])) |>
                            httr2::req_headers(
                                noredirect = TRUE,
                                type = "URL"
                            ) |>
                            httr2::req_throttle(
                                rate = 30 / 60
                            ) |>
                            httr2::req_retry(
                                max_tries = 5
                            ) |>
                            req_error(
                                is_error = function(e) {
                                    FALSE
                                }
                            ) |>
                            httr2::req_perform() |>
                            httr2::resp_status()
                        return(status == 200)
                    },
                    error = function(e) {
                        return(NA)
                    }
                )
            }
        }
    )

    result <- dois
    result[] <- NA
    result <- as.logical(result)
    names(result) <- dois

    result[dois_to_check] <- result_dois_checked

    if (!is.null(cache)) {
        result[dois[(dois %in% cache)]] <- TRUE
        cache <- c(cache, result[result])
        cache <- cache[!duplicated(cache)]
        saveRDS(cache, cache_file)
    }
    return(result)
}
