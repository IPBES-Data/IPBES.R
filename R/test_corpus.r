#' Test Corpus
#'
#' This function tests the parquet files from a specified directory and prints a message for each file.
#' The function does not return anything, the essential are the disgnostig messages
#' @importFrom arrow read_parquet
#' @param path The path to the directory containing the parquet files.
#' @return NULL
#'
#' @export
#'
#' @examples
#' test_corpus()
#'
#' @keywords internal
test_corpus <- function(
    path = file.path("data", "corpus")) {
    p <- list.files(
        path = "data/corpus",
        pattern = ".parquet$",
        recursive = TRUE,
        full.names = TRUE
    )
    oldOpt <- options(warn = 1)
    try(
        invisible(
            lapply(
                p,
                function(x) {
                    message(x)
                    read_parquet(x)
                    invisible(NULL)
                }
            )
        )
    )
    options(oldOpt)
}
