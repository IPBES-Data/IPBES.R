#' Get Corpus
#'
#' This function retrieves data from OpenAlex and saves it to an arrow database using the parquet file format.
#' The database is partitioned by `publication_year``.
#' In case of a crash during the download, the function, when ryn again,  will skip the existing files and continue with
#' the next year.
#' @param select A character vector specifying the columns to select from the data. Default is a set of predefined
#'   columns.
#' @param verbose A logical value indicating whether to display verbose output. Default is FALSE.
#' @param arrow_directory The path where the parquet files will be saved. Default is "corpus".
#' @param mc_cores The number of CPU cores to use for parallel processing. Default is 8.
#' @param ... Additional filter arguments to be passed to the `openalexR::oa_fetch` function.
#'
#' @md
#'
#' @importFrom openalexR oa_fetch
#' @importFrom pbmcapply pbmclapply
#' @importFrom parquetize write_parquet_at_once
#'
#' @return path to the arrow directory
#'
#' @examples
#' \dontrun{
#' get_corpus(title_and_abstract.search = "biodiversity")
#' }
#'
#' @autoglobal
#' @export
get_corpus <- function(select = c(
                           "id",
                           "doi",
                           "authorships",
                           "publication_year",
                           "title",
                           "abstract_inverted_index",
                           "topics"
                       ),
                       verbose = FALSE,
                       arrow_directory = "corpus",
                       mc_cores = 8,
                       ...) {
    dir.create(
        path = arrow_directory,
        showWarnings = FALSE,
        recursive = TRUE
    )

    years <- openalexR::oa_fetch(
        entity = "works",
        ...,
        group_by = "publication_year",
        paging = "cursor",
        verbose = verbose
    )$key

    years <- years[
        !(years %in%
            (
                list.files(
                    path = arrow_directory,
                    pattern = paste0("^", "publication_year", "="),
                    full.names = FALSE,
                    recursive = FALSE
                ) |>
                    gsub(
                        pattern = paste0("^", "publication_year", "="),
                        replacement = ""
                    )
            )
        )
    ]

    pbmcapply::pbmclapply(
        sample(years),
        function(y) {
            data <- openalexR::oa_fetch(
                entity = "works",
                ...,
                publication_year = y,
                options = list(
                    select = select
                ),
                ...,
                verbose = verbose
            )
            data$author <- abbreviate_authors(data)
            suppressMessages(
                parquetize::write_parquet_at_once(
                    data = data,
                    path_to_parquet = arrow_directory,
                    partition = "yes",
                    partitioning = "publication_year"
                )
            )
        },
        mc.cores = mc_cores,
        mc.preschedule = FALSE
    )
}
