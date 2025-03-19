#' Get Zotero Group Data
#'
#' This function retrieves data from a Zotero group using the Zotero API.
#'
#' This function uses the Zotero API to retrieve data from a Zotero group. It downloads the data in an rdf format
#' and saves it in batches of 100 records to the folder specified by the `path` parameter. The fplder specified will always be deleted before processing.
#'
#' For further information on the API see h[ttps://www.zotero.org/support/dev/web_api/v3/start](https://www.zotero.org/support/dev/web_api/v3/start).
#' Note: To use this function, you need to have the \pkg{httr2} and \pkg{stringr} packages installed.
#'
#' @param group_id The ID of the Zotero group.
#' @param path The path to save the retrieved data.
#' @param output_format The format of the rdf file. Supported is `rdf_zotero`, `rdf_dc` and `rdf_bibliontology`
#' (see \@link[https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats]{here} for all
#' supported rdf formats). Default is `rdf_zotero`.
#'
#' @param api_key API key for Zotero. Only needed for private groups.
#' @importFrom httr2 request req_headers req_url_query req_perform resp_headers resp_body_string req_retry resp_status
#' @importFrom utils read.csv write.table
#'
#' @return If `return_data` The `path`` where the data is saved.
#'
#' @examples
#' # Retrieve data from Zotero group with ID 2352922 (IPBES IAS Assessment Bibliograhy) and save it
#' # in the file named "zotero_data.csv"
#'
#' zotero_get_group_rdf(2352922, "zotero_data")
#'
#' @md
#'
#' @export
zotero_get_group_rdf <- function(
    group_id = 2352922,
    path = tempfile(),
    output_format = "rdf_zotero",
    api_key = NULL) {
  ##

  if (!(output_format %in% c("rdf_zotero", "rdf_dc", "rdf_bibliontology"))) {
    stop("output_format must be one of `rdf_zotero`, `rdf_dc` and `rdf_bibliontology` ")
  }

  if (dir.exists(path)) {
    unlink(filpathe)
  } else {
    dir.create(
      path, 
      showWarnings = FALSE, 
      recursive = TRUE
    )
  }


  api_endpoint <- "https://api.zotero.org/"

  url <- paste0(
    api_endpoint,
    "groups/", group_id, "/",
    "items"
  )

  req <- url |>
    httr2::request() |>
    httr2::req_retry(
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 503)
      },
      max_tries = 10
    )

  req <- req |>
    httr2::req_headers(
      "Zotero-API-Version" = 3
    )

  req <- req |>
    httr2::req_url_query(
      "format" = output_format,
      "limit" = 100,
      "start" = 0
    )

  if (!is.null(api_key)) {
    req <- req |>
      httr2::req_url_query(
        "key" = api_key
      )
  }

  next_start <- 0

  tmp_path <- tempfile()
  on.exit(
    unlink(tmp_path, recursive = TRUE)
  )
  dir.create(
    tmp_path,
    showWarnings = FALSE,
    recursive = TRUE
  )

  repeat {
    message("Downloading 100 records starting at record ", next_start, " ...")
    old_start <- next_start

    req <- req |>
      httr2::req_url_query(
        "start" = next_start
      )

    resp <- req |>
      httr2::req_perform()

    next_start <- resp |>
      httr2::resp_headers(filter = "link") |>
      as.character() |>
      strsplit(split = "\",") |>
      unlist() |>
      grep(pattern = "\"next", value = TRUE) |>
      gsub(pattern = ".*start=([0-9]+).*", replacement = "\\1")

    if (length(next_start) == 0) {
      break()
    }

    writeLines(
      httr2::resp_body_string(resp),
      con = file.path(tmp_path, paste0(output_format, "_", old_start, "_", next_start, ".rdf"))
    )
  }

  file.copy(
    from = tmp_path,
    to = path,
    recursive = TRUE
  )

  return(path)
}
