#' Unserialize Arrow
#'
#' This function unserializes the vector `x` and returns a vector containing the extracted values.
#'
#' @param x A character vector containing serialized values, at the moment 'topics' and 'author' columns.
#' @return A vector of lists containing the unseerialu=ized data
#' @importFrom base64enc base64decode
#'
#' @export
#'
#' @examples
unserialize_arrow <- function(x) {
    result <- vector("list", length(x))

    for (i in seq_along(x)) {
        result[i] <- if (is.na(x[i])) {
            list(as.data.frame(NA))
        } else {
            list(
                x[i] |>
                    base64enc::base64decode() |>
                    unserialize()
            )
        }
    }
    return(result)
}
