oa_get_schemes <- function(
    url = "https://raw.githubusercontent.com/ourresearch/openalex-documentation-scripts/main/openalex-pg-schema.sql") {
  ################################
  stop("Work in progress - not working and dopubtfully useful!")

  library(duckdb)
  library(dplyr)

  sql_file <- tempfile()
  utils::download.file(
    url = url,
    destfile = sql_file,
    quiet = TRUE
  )

  # Read the SQL file and extract the list of schemas

  sql <- readLines(sql_file)
  sql <- sql[!grepl(pattern = "--", x = sql)]
  sql <- sql[sql != ""]
  sql <- paste(sql, collapse = " ")
  sql <- strsplit(
    sql,
    split = ";"
  ) |>
    unlist(recursive = FALSE)

  sql <- sql[grepl(pattern = "CREATE TABLE", x = sql)]

  sql <- gsub(
    "openalex\\.",
    "",
    sql
  )

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  dbExecute(con, "INSTALL JSON")
  dbExecute(con, "LOAD JSON")

  lapply(
    sql,
    function(s) {
      message("Executing ", s)
      dbExecute(con, s)
    }
  )

  dbExecute(con, "EXPORT DATABASE 'parquet' (FORMAT PARQUET);")

  dbDisconnect(con)
}
