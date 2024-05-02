#' Filter corpus by IDs and save as Arrow datasets
#'
#' This function filters a corpus of data by a set of IDs and saves the filtered data as Arrow datasets.
#'
#' The function performs the following steps:
#' 1. Retrieves the list of year directories within the \code{arrow_dir} path.
#' 2. Extracts the publication years from the directory names.
#' 3. Iterates over each year directory and performs the following operations:
#'    - Reads the corpus dataset for the current year.
#'    - Filters the dataset by the provided \code{filter_ids}.
#'    - If the filtered dataset is not empty, it is mutated to include the publication year and saved as an Arrow dataset in the \code{arrow_filter_dir} path.
#' @param arrow_dir The directory path where the corpus Arrow datasets are located. default: `file.path(".", "data", "corpus"`
#' @param arrow_filter_dir The directory path where the filtered Arrow datasets will be saved. default: `file.path(".", "data", "corpus_filtered")`
#' @param filter_ids A vector of IDs to filter the corpus by.
#'
#' @importFrom dplyr collect filter mutate
#' @importFrom arrow write_dataset
#' @importFrom pbapply pblapply
#'
#' @md
#'
#' @export
corpus_filter_ids <- function(
    arrow_dir = file.path(".", "data", "corpus"),
    arrow_filter_dir = file.path(".", "data", "corpus_filtered"),
    filter_ids = NULL) {
    if (is.null(filter_ids)) {
        stop("The filter_ids argument must be provided.")
    }

    year_dirs <- list.dirs(
        path = arrow_dir,
        full.names = TRUE,
        recursive = FALSE
    )
    years <- basename(year_dirs) |>
        gsub(
            pattern = "publication_year=",
            replacement = ""
        )
    ys <- seq_len(length(year_dirs))

    pbapply::pblapply(
        ys,
        function(y) {
            data <- read_corpus(year_dirs[[y]]) |>
                dplyr::collect() |>
                dplyr::filter(id %in% filter_ids)
            if (nrow(data) > 0) {
                data |>
                    dplyr::mutate(publication_year = as.integer(years[[y]])) |>
                    arrow::write_dataset(
                        path = arrow_filter_dir,
                        partitioning = "publication_year",
                        format = "parquet",
                        existing_data_behavior = "overwrite"
                    )
            }
        }
    ) |> invisible()

    
}
