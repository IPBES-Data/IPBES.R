library(httr2)



#' Check if DOIs exist
#'
#' This function checks if a given list of DOIs exist by sending HTTP requests to the DOI resolver.
#'
#' @details This function checks if DOIs do exist, i.e. can be resolved. Two different methods are provided:
#'   - doi.org: The default and recommended approach. The dois are verified, one-by-one, using the official authoratative [doi.org](https://doi.org) resolver.
#'     This is a robust approach and malformed dDOIs are handled gracefully, i.e. as non existing.
#'   - OpenAlex: The dois are used as filter in OpenAlex and one field is retrieved if they do exist on OpenAlex. This is not
#'     forgiving for malformed DOIs (i.e. with an error). Furthermore, OpenAlex does not has all DOIs.
#' The dois are cleaned, i.e. the resolver are removed, before processing.
#' @param dois character vector: A vector of DOIs to be validated. Resolver will be removed.
#' @param cache_file An `.rds` file containing the results from a previous check.
#' @param use string of length 1: The backend to be used for the verification of existence. Each has advantages or disadvantages.
#'   - **`doi.org`**: has the most DOIs; is more stable towards mal-formed DOIs
#'   - **`OpenAlex`**: does not have all DOIs but returns if the DOI is in OpenAlex; crashes when DOI is mal-formed
#'
#' @return A named logical vector indicating whether each DOI does exist or not, names are the dois.
#'
#' @md
#' @examples
#' dois <- c(
#'   "sbcd1234", # not existing
#'   "10.1234/abcd", # not existing
#'   "https://doi.org/10.1002/jcb.23190", # existing with resolver
#'   "10.47366/sabia.v5n1a3", # existing
#'   "10.1002/ece3.10852", # existing
#'   "10.1002/ece3.10853", # existing
#'   "10.1002/ece3.10854", # existing
#'   "10.1002/ece3.10855", # existing
#'   "10.1002/ece3.10856", # existing
#'   "10.1002/ece3.10857" # existing
#' )
#' doi_exists(dois, use = "doi.org")
#' # Output: sbcd1234 10.1234/abcd 10.1002/jcb.23190 10.47366/sabia.v5n1a3 10.1002/ece3.10852 10.1002/ece3.10853 10.1002/ece3.10854 10.1002/ece3.10855 10.1002/ece3.10856 10.1002/ece3.10857
#' #         FALSE         FALSE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE
#'
#' doi_exists(dois, use = "OpenAlex")
#' # Output: sbcd1234 10.1234/abcd 10.1002/jcb.23190 10.47366/sabia.v5n1a3 10.1002/ece3.10852 10.1002/ece3.10853 10.1002/ece3.10854 10.1002/ece3.10855 10.1002/ece3.10856 10.1002/ece3.10857
#' #         FALSE         FALSE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE          TRUE
#'
#' @importFrom openalexR oa_fetch
#' @importFrom pbapply pblapply
#' @importFrom httr2 request req_headers req_throttle req_retry req_error req_perform resp_body_json

#' @export
doi_exists <- function(dois, cache_file = NULL, use = "doi.org") {
  if (is.null(dois)) {
    warning("dois are NULL - returning logical(0)")
    return(logical(0))
  }
  if (length(dois) == 0) {
    warning("dois of length zero - returning logical(0)")
    return(logical(0))
  }

  # remove resolver part from dois and exclude duplicates
  dois <- doi_clean(dois)
  dois_org <- dois
  dois <- unique(dois)

  # read cache and select only dois to check which are not in the cache or did not exist
  if (!is.null(cache_file) && file.exists(cache_file)) {
    message(paste0("Using cache from ", cache_file))
    cache <- readRDS(cache_file)
    dois_to_check <- dois[!dois %in% names(cache[cache])]
  } else {
    cache <- logical(0)
    dois_to_check <- dois
  }

  ## Check the DOIs
  if (use == "OpenAlex") {
    suppressWarnings(
      do_exist <- openalexR::oa_fetch(
        entity = "work",
        doi = dois,
        verbose = TRUE,
        options = list(select = "doi")
      ) |>
        unlist() |>
        unname() |>
        doi_clean() |>
        `%in%`(x = dois)
    )
    names(do_exist) <- dois
  } else if (use == "doi.org") {
    do_exist <- pbapply::pblapply(
      dois,
      function(doi) {
        response <- NULL
        try(
          response <- httr2::request(paste0("https://doi.org/api/handles/", doi)) |>
            #   httr2::req_throttle(
            #     rate = 90 / 60
            #   ) |>
            httr2::req_retry(
              max_tries = 5
            ) |>
            httr2::req_error(
              is_error = function(e) {
                FALSE
              }
            ) |>
            httr2::req_perform() |>
            httr2::resp_body_json()
        )
        if (is.null(response)) {
          message("Malformed DOI: ", doi)
          return(FALSE)
        } else {
          return(response$responseCode == 1)
        }
      }
    ) |>
      unlist()
    names(do_exist) <- dois
  }

  do_exist <- names(do_exist)[do_exist]

  do_exist <- as.logical(dois_org %in% do_exist)
  names(do_exist) <- dois_org

  if (!is.null(cache_file)) {
    c(cache, do_exist[!duplicated(names(do_exist))]) |>
      unique() |>
      saveRDS(file = cache_file)
  }

  return(do_exist)
}
