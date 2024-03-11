#' Unserialize Arrow
#'
#' This function unserializes the 'topics' and 'author' columns of a data frame using base64 decoding.
#'
#' @param data A data frame containing serialized 'topics' and 'author' columns.
#' @return A modified data frame with unserialized 'topics' and 'author' columns.
#' @importFrom purrr map
#' @importFrom base64enc base64decode
#'
#' @export
#'
#' @examples
#' data <- data.frame(topics = list("dHlwZTE=", "dHlwZTI="), author = list("YXV0aG9yMQ==", "YXV0aG9yMg=="))
#' unserialize_arrow(data)
unserialize_arrow <- function(data) {
    data |>
        mutate(
            topics = purrr::map(topics, ~ .x |>
                base64enc::base64decode() |>
                unserialize()),
            author = purrr::map(author, ~ .x |>
                base64enc::base64decode() |>
                unserialize())
        )
}
