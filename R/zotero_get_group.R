#' Get Zotero Group Data
#'
#' This function retrieves data from a Zotero group using the Zotero API.
#'
#' This function uses the Zotero API to retrieve data from a Zotero group. It downloads the data in CSV format
#' and saves it to a file specified by the `file`` parameter. The function retrieves the data in batches of 100
#' records at a time and appends them to the file. If `overwrite`` is set to `TRUE``, an existing file with
#' the same name will be overwritten. The function also provides options to return the data as a data frame and delete
#' the file after retrieving the data.
#'
#' For further information on the API see h[ttps://www.zotero.org/support/dev/web_api/v3/start](https://www.zotero.org/support/dev/web_api/v3/start).
#' Note: To use this function, you need to have the \pkg{httr2} and \pkg{stringr} packages installed.
#'
#' @param group_id The ID of the Zotero group.
#' @param file The file path to save the retrieved data. The output format extension will be added automatically.
#' @param overwrite Logical indicating whether to overwrite an existing file with the same name.
#' @param output_format The format of the output file. Supported is `parquet` and `csv`. Default is "csv".
#' @param return_data Logical indicating whether to return the data as a data frame.
#'
#' @param api_key API key for Zotero. Only needed for private groups.
#' @importFrom httr2 request req_headers req_url_query req_perform resp_headers resp_body_string req_retry resp_status
#' @importFrom utils read.csv write.table
#'
#' @return If `return_data`` is` TRUE`, the function returns a data frame containing the retrieved data.
#'   Otherwise, it returns the file path where the data is saved.
#'
#' @examples
#' # Retrieve data from Zotero group with ID 2352922 (IPBES IAS Assessment Bibliograhy) and save it
#' # in the file named "zotero_data.csv"
#'
#' zotero_get_group(2352922, "zotero_data.csv")
#'
#' # Retrieve the same data and return it as a as a data frame. The csv file is deleted.
#'
#' data <- zotero_get_group(2352922, return_data = TRUE, delete_file = TRUE)
#'
#' @md
#'
#' @export
zotero_get_group <- function(
    group_id = 2352922,
    file = tempfile(fileext = ".csv"),
    overwrite = FALSE,
    return_data = FALSE,
    output_format = "csv",
    api_key = NULL) {
  ##

  if (!(output_format %in% c("csv", "parquet"))) {
    stop("output_format must be one of 'csv' or 'parquet'")
  }

  if (output_format == "parquet") {
    file <- paste0(file, ".parquet")
  } else {
    file <- paste0(file, ".csv")
  }

  if (file.exists(file)) {
    if (overwrite) {
      message("Overwriting existing file ", file)
      unlink(file)
    } else {
      stop("File ", file, " already exists and would be overwritten!")
    }
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
      "format" = "csv",
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

  tmp_file <- tempfile()
  on.exit(unlink(tmp_file))
  repeat {
    message("Downloading 100 records starting at record ", next_start, " ...")
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

    resp |>
      httr2::resp_body_string() |>
      textConnection() |>
      read.csv() |>
      write.table(
        file = tmp_file,
        append = file.exists(tmp_file),
        quote = TRUE,
        sep = ",",
        dec = ".",
        qmethod = "double",
        row.names = FALSE,
        col.names = !file.exists(tmp_file)
      )
  }

  if (return_data) {
    result <- read.csv(tmp_file)
  } else {
    if (output_format == "csv") {
      file.copy(
        from = tmp_file,
        to = file
      )
      result <- file
    } else if (output_format == "parquet") {
      read.csv(tmp_file) |>
        arrow::write_parquet(
          sink = file,
        )
      result <- file
    }
  }

  return(result)
}
