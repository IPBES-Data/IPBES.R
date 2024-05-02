oa_snapshot_to_arrow <- function(
    snapshot_dir = "/Volumes//openalex//openalex-snapshot/",
    data_set = c("authors", "concepts", "funders", "institutions", "publishers", "sources", "works", "merged_ids"),
    arrow_dir = "/Volumes//openalex/arrow/") {
    stop("NOT WORKING")

    json_dir <- file.path(snapshot_dir, "data", data_set)

    dir.create(arrow_dir, recursive = TRUE, showWarnings = FALSE)

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "~/tmp/temp.duckdb", read_only = FALSE)

    paste0(
        "INSTALL json"
    ) |>
        DBI::dbExecute(conn = con)


    paste0(
        "LOAD json"
    ) |>
        DBI::dbExecute(conn = con)

    x <- paste0(
        "SELECT ",
        "       * ",
        "   FROM ",
        "       read_ndjson('", json_dir, "/*/*.gz')"
        # "(FORMAT PARQUET, COMPRESSION 'SNAPPY', PARTITION_BY 'publication_year')"
    ) |>
        DBI::dbGetQuery(conn = con)


    paste0(
        "COPY ( ",
        "   SELECT ",
        "       * ",
        "   FROM ",
        "       read_ndjson('", json_dir, "/*/*.gz')",
        ") TO '", arrow_dir, "' ",
        "(FORMAT PARQUET, COMPRESSION 'SNAPPY')"
        # "(FORMAT PARQUET, COMPRESSION 'SNAPPY', PARTITION_BY 'publication_year')"
    ) |>
        DBI::dbExecute(conn = con)

    duckdb::dbDisconnect(con, shutdown = TRUE)
}
