#' Get Corpus
#'
#' This function opens a dataset from a specified path and returns a corpus object.
#'
#' @param path The path to the dataset.
#' @param unify_schemas Logical indicating whether to unify the schemas of the datasets.
#'   `TRUE` is the safer option, but takes more time to read the first time,Default is FALSE.
#' @importFrom arrow open_dataset
#'
#' @export
#'
#' @return A corpus object.
#'
#' @md
#'
#' @examples
#' corpus_read("data/corpus/")
corpus_read <- function(
    path = file.path("data", "corpus"),
    unify_schemas = FALSE) {
    con <- arrow::open_dataset(
        sources = path,
        unify_schemas = unify_schemas
    )
    return(con)
}

#' Alias to arrow_read_corpus
#'
#' @description This function is an alias to \code{\link{arrow_read_corpus}}.
#' For more details, see the documentation for \code{\link{arrow_read_corpus}}.
#'
#' @rdname arrow_read_corpus
#' @export
read_corpus <- corpus_read
