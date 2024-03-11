#' Compact
#'
#' This function removes newlines, asterisks, extra spaces, and unnecessary spaces around parentheses from a string.
#'
#' @param x A character string.
#' @return A modified string with newlines, asterisks, extra spaces, and unnecessary spaces around parentheses removed.
#'
#' @export
#'
#' @examples
#' x <- "This is a\nstring with *extra* spaces (and unnecessary spaces)."
#' compact(x)
compact <- function(x) {
    x |>
        gsub(pattern = "\n", replacement = " ") |>
        gsub(pattern = "\\*", replacement = "") |>
        gsub(pattern = "\\s+", replacement = " ") |>
        gsub(pattern = "\\( ", replacement = "(") |>
        gsub(pattern = " )", replacement = ")")
}
