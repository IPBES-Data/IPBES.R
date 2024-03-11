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
#' get_corpus("data/corpus/")
read_corpus <- function(
    path = file.path("data", "corpus"),
    unify_schemas = FALSE) {
    con <- arrow::open_dataset(
        sources = path,
        unify_schemas = unify_schemas
    )
    return(con)
}
