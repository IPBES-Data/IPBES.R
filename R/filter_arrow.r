## TODO

filter_arrow <- function(
    arrow_dir = file.path(".", "data", "corpus"),
    arrow_filter_dir = file.path(".", "data", "corpus_tca"),
    filter_ids = readRDS("data/ids_subs_tca.rds")) {
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
                dplyr::filter(id %in% arrow_tca_ids)
            if (nrow(data) > 0) {
                data |>
                    dplyr::mutate(publication_year = as.integer(years[[y]])) |>
                    arrow::write_dataset(
                        path = arrow_tca_dir,
                        partitioning = "publication_year",
                        format = "parquet",
                        existing_data_behavior = "overwrite"
                    )
            }
        }
    )
}
