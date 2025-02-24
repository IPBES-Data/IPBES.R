library(httr2)



#' Check if DOIs exist
#'
#' This function checks if a given list of DOIs exist by sending HTTP requests to the DOI resolver.
#'
#' @details This function uses the httr package to send HTTP GET requests to the DOI resolver and checks the response status code. A status code of 200 indicates that the DOI exists, while any other status code indicates that the DOI does not exist.
#'
#' The dois are cleaned, i.e. the resolver are removed, before processing.
#' @param dois A vector of DOIs to be validated. Resolver will be removed.
#' @param cache_file A file name of the cache to be used, i.e. the confirmed existing dois. The format is a character vector with the DOIs which exist. If the cache exist, it will be updated at the end. Temporary caches will be written after 100 checks.
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
    # remove resolver part from dois
    dois <- doi_clean(dois)

    if (is.null(cache_file)) {
        cache <- NULL
        dois_to_check <- dois
    } else {
        if (file.exists(cache_file)) {
            cache <- readRDS(cache_file)
            dois_to_check <- dois[!(dois %in% cache)]
        } else {
            cache <- NULL
            dois_to_check <- dois
        }
    }

    result <- logical(length(dois))
    result[] <- NA
    names(result) <- dois

    if (length(dois_to_check) == 0) {
        result[] <- TRUE
        return(result)
    }

    result_dois_checked <- data.frame(
        doi = dois_to_check,
        exists = FALSE
    )

    total <- nrow(dois_to_check)
    for (j in seq_along(dois_to_check)) {
        # Print progress
        message("\nProgress: ", j, " of ", total, "\n")

        if (!is.na(result_dois_checked$doi[j])) {
            try({
                response <- httr2::request(paste0("https://doi.org/api/handles/", result_dois_checked$doi[j])) |>
                    # httr2::req_headers(
                    #     noredirect = TRUE,
                    #     type = "URL"
                    # ) |>
                    httr2::req_throttle(
                        rate = 90 / 60
                    ) |>
                    httr2::req_retry(
                        max_tries = 5
                    ) |>
                    # httr2::req_error(
                    #     is_error = function(e) {
                    #         FALSE
                    #     }
                    # ) |>
                    httr2::req_perform() |>
                    httr2::resp_body_json()
                result_dois_checked$exists[j] <- (response$responseCode == 1)

                # Save temporary cache
                if ((!is.null(cache_file)) & (j %% 100 == 0)) {
                    message("\nSaving temporary cache to ", paste0(cache_file, ".TMP.", j, "\n"))
                    saveRDS(
                        result_dois_checked[result_dois_checked$exists, "doi"],
                        paste0(cache_file, ".TMP.", j)
                    )
                }
            })
        }
    }


    cache <- c(cache, result_dois_checked$doi[result_dois_checked$exists]) |>
        unique()

    doi_not_found <- result_dois_checked$doi[!result_dois_checked$exists]

    result[names(result) %in% doi_not_found] <- FALSE

    result[names(result) %in% cache] <- TRUE

    if (!is.null(cache_file)) {
        cache |>
            unique() |>
            saveRDS(file = cache_file)
    }

    return(result)
}
