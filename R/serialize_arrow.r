#' Serialize Arrow
#'
#' This function serializes the 'topics' and 'author' columns of a data frame using base64 encoding.
#'
#' @param data A data frame containing 'topics' and 'author' columns.
#' @return A modified data frame with serialized 'topics' and 'author' columns.
#' @importFrom purrr map_chr
#' @importFrom base64enc base64encode
#'
#' @export
#'
#' @examples
#' data <- data.frame(topics = list("topic1", "topic2"), author = list("author1", "author2"))
#' serialize_arrow(data)
serialize_arrow <- function(data) {
    data |>
        mutate(
            topics = ifelse(
                is.na(topics),
                as.character(NA),
                purrr::map_chr(topics, ~ serialize(.x, NULL) |> base64enc::base64encode())
            ),
            author = ifelse(
                is.na(author),
                as.character(NA),
                purrr::map_chr(author, ~ serialize(.x, NULL) |> base64enc::base64encode())
            )
        )
}
