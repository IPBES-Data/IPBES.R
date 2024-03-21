#' Get Corpus
#'
#' This function opens a dataset from a specified path and returns a corpus object.
#'
#' @param path The path to the dataset.
#' @importFrom arrow open_dataset
#'
#' @export
#'
#' @return A corpus object.
#' @examples
#' arrow_get_corpus("data/corpus/")
arrow_read_corpus <- function(
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
read_corpus <- arrow_read_corpus
