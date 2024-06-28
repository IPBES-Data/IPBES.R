#' Extract topics from works
#'
#' This function extracts the topics from a given data frame.
#' The `extract_authors` function takes a data frame of authors' information and extracts the institution country codes.
#' By default, it returns a vector containing only the first institution country code for the first author.
#' If `first_only` is set to FALSE, it returns a vector containing the institution country codes for all authors.
#' If an author has no institution country code or if it is missing, NA is returned.
#' @param data A data frame containing these three columns:
#'     - ìd`: the work id
#'     - `publication_year`: the publication year
#'     - `topics`: a list of topics
#' @param names A character vector specifying the names of the topics to extract.
#'   Default is `c("topic", "subfield", "field", "domain")`.
#' @param unserialize A logical value indicating whether to unserialize the topics.
#'   If `NULL` it is set to `TRUE` if `class(data$topics)`, otherwise `FALSE`.
#'   Default is NULL.
#'
#' @return A `data.frame with two columns:
#'    - `works_id`: the work id
#'   - `publication_year`: the publication year
#'   - `i`: a number indicating if the topic is the primary, secondary or tertiary topic
#'   - `score`: the score of the topic
#'   - `name`: specifying if the column contains the `topic`, `subfield`, `field` or `domain`
#'   - `id`: the id of the topic
#'   - `display_name`: the name of the topic
#'
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'     id = c(1, 2, 3),
#'     publication_year = c(2020, 2021, 2022),
#'     topics = list(
#'         data.frame(
#'             i = c(1, 2),
#'             score = c(0.8, 0.6),
#'             name = c("topic1", "topic2"),
#'             id = c(101, 102),
#'             display_name = c("Topic 1", "Topic 2")
#'         ),
#'         data.frame(
#'             i = c(1, 2, 3),
#'             score = c(0.9, 0.7, 0.5),
#'             name = c("topic3", "topic4", "topic5"),
#'             id = c(103, 104, 105),
#'             display_name = c("Topic 3", "Topic 4", "Topic 5")
#'         ),
#'         data.frame()
#'     )
#' )
#'
#' extract_topics(data)
#'
#' # Output:
#' # A tibble: 5 x 7
#' #   works_id publication_year     i score name     id display_name
#' #      <dbl>            <dbl> <dbl> <dbl> <chr> <dbl> <chr>
#' # 1        1             2020     1   0.8 topic1   101 Topic 1
#' # 2        1             2020     2   0.6 topic2   102 Topic 2
#' # 3        2             2021     1   0.9 topic3   103 Topic 3
#' # 4        2             2021     2   0.7 topic4   104 Topic 4
#' # 5        2             2021     3   0.5 topic5   105 Topic 5
#' }
#' 
extract_topics <- function(
    data,
    names = c("topic", "subfield", "field", "domain"),
    unserialize = NULL) {
    #

    if (is.null(unserialize)) {
        unserialize <- is.character(data$topics[[1]])
    }

    if (unserialize) {
        topics <- unserialize_arrow(data$topics)
    } else {
        topics <- data$topics
    }

    topics <- lapply(
        1:length(topics),
        function(i) {
            if (!inherits(topics[[i]], "data.frame")) {
                return(NULL)
            } else if (isTRUE(nrow(topics[[i]]) == 0)) {
                return(NULL)
            } else {
                return(
                    cbind(
                        work_id = data$id[[i]],
                        publication_year = data$publication_year[[i]],
                        topics[[i]]
                    )
                )
            }
        }
    ) |>
        do.call(what = rbind) |>
        tibble::as_tibble() |>
        dplyr::filter(
            name %in% names
        )

    return(topics)
}
