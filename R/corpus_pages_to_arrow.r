#' Convert Downloaded Corpus Data to Arrow Format
#'
#' This function converts downloaded data in RDS format to Arrow format.
#'
#' @param pages_dir The directory where the downloaded data is stored.
#' @param arrow_dir The directory where the converted Arrow data will be stored.
#' @param continue Logical indicating whether to continue from where the conversion was left off.
#' @param delete_arrow_dir Logical indicating whether to delete the existing Arrow directory before conversion.
#' @param verbose Logical indicating whether to display messages.
#' @param mc_cores The number of cores to be used for parallel processing. Default is 3.
#'   This is limited by memory requirements.
#'
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom openalexR works2df
#' @importFrom arrow write_dataset
#'
#' @export
corpus_pages_to_arrow <- function(
    pages_dir = file.path(".", "data", "pages"),
    arrow_dir = file.path(".", "data", "corpus"),
    continue = TRUE,
    delete_arrow_dir = FALSE,
    verbose = FALSE,
    mc_cores = 3
    #
    ) {
    if (!dir.exists(pages_dir)) {
        stop("The pages directory does not exist.")
    }

    if (delete_arrow_dir) {
        unlink(
            arrow_dir,
            recursive = TRUE
        )
    }

    if (!dir.exists(arrow_dir)) {
        dir.create(
            path = arrow_dir,
            showWarnings = FALSE,
            recursive = TRUE
        )
    }

    years <- list.dirs(
        path = pages_dir,
        full.names = TRUE,
        recursive = FALSE
    )


    if (continue) {
        years_done <- list.dirs(
            path = arrow_dir,
            full.names = TRUE,
            recursive = FALSE
        )

        years <- years[
            !(
                gsub(
                    x = years,
                    pattern = paste0("^", pages_dir, "/pages_publication_year="),
                    replacement = ""
                ) %in% gsub(
                    x = years_done,
                    pattern = paste0("^", arrow_dir, "/publication_year="),
                    replacement = ""
                )
            )
        ]
    }

    pbapply::pblapply(
        sample(years),
        function(year) {
            if (verbose) {
                message("\n     Processing year ", year, " ...\n")
            }
            sets <- list.files(
                path = year,
                pattern = "^set_",
                full.names = TRUE,
                recursive = TRUE
            )
            data <- parallel::mclapply(
                sets,
                function(set) {
                    data <- readRDS(file.path(set)) |>
                        openalexR::works2df(verbose = FALSE)

                    data$author_abbr <- IPBES.R::abbreviate_authors(data)
                    data$set <- set |>
                        basename() |>
                        gsub(pattern = "^set_", replacement = "") |>
                        gsub(pattern = ".rds$", replacement = "")

                    data <- serialize_arrow(data)

                    arrow::write_dataset(
                        data,
                        path = arrow_dir,
                        partitioning = c("publication_year", "set"),
                        format = "parquet",
                        existing_data_behavior = "overwrite"
                    )
                },
                mc.cores = mc_cores
            )
        }
    ) |>
        invisible()
}
