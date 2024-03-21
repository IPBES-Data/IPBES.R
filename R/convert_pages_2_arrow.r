#' Convert Downloaded Corpus Data to Arrow Format
#'
#' This function converts downloaded data in RDS format to Arrow format.
#'
#' @param pages_dir The directory where the downloaded data is stored.
#' @param arrow_dir The directory where the converted Arrow data will be stored.
#' @param continue Logical indicating whether to continue from where the conversion was left off.
#' @param delete_arrow_dir Logical indicating whether to delete the existing Arrow directory before conversion.
#' @param mc_cores The number of cores to be used for parallel processing. Default is 3.
#'   This is limited by memory requirements.
#'
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom openalexR works2df
#' @importFrom arrow write_dataset
#'
#' @export
convert_pages_2_arrow <- function(
    pages_dir = file.path(".", "data", "pages"),
    arrow_dir = file.path(".", "data", "corpus"),
    continue = TRUE,
    delete_arrow_dir = FALSE,
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
            pages <- list.files(
                path = year,
                pattern = "^page_",
                full.names = TRUE,
                recursive = TRUE
            )
            data <- parallel::mclapply(
                pages,
                function(page) {
                    data <- readRDS(file.path(page))$results |>
                        openalexR::works2df(verbose = FALSE)

                    data$author_abbr <- IPBES.R::abbreviate_authors(data)
                    data$page <- page |>
                        basename() |>
                        gsub(pattern = "^page_", replacement = "") |>
                        gsub(pattern = ".rds$", replacement = "")

                    data <- serialize_arrow(data)


                    arrow::write_dataset(
                        data,
                        path = arrow_dir,
                        partitioning = c("publication_year", "page"),
                        format = "parquet",
                        existing_data_behavior = "overwrite"
                    )

                    # p <- readRDS(file.path(page))$results |>
                    #     openalexR::works2df(verbose = FALSE)
                    # p$author_abbr <- IPBES.R::abbreviate_authors(p)
                    # return(p)
                },
                mc.cores = mc_cores
            ) # |>
            #     do.call(what = rbind)

            # saveRDS(
            #     data,
            #     file = file.path(paste0(year, ".rds"))
            # )

            # data <- serialize_arrow(data)

            # arrow::write_dataset(
            #     data,
            #     path = arrow_dir,
            #     partitioning = "publication_year",
            #     format = "parquet",
            #     existing_data_behavior = "overwrite"
            # )
        }
    )
}
