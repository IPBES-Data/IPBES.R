#' Convert Snowball to result
#'
#' This function takes a snowball object and a filename, and converts the snowball object to an
#' data.frame containing bibliometric information.
#' @param snowball A snowball object containing the data to be converted.
#'
#' @return The \code{data.frame} generated
#' @export
#'
#' @importFrom openalexR snowball2df
#' @importFrom dplyr select mutate arrange rename full_join relocate filter desc
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' to_data_frame(snowball, "example.result")
#' }
to_data_frame <- function(
    snowball) {
    flat_snow <- openalexR::snowball2df(snowball)
    flat_snow$author <- IPBES.R::abbreviate_authors(flat_snow)

    no_edges <- snowball$edges |>
        unlist() |>
        table() |>
        sort() |>
        as.data.frame() |>
        dplyr::rename(
            no_connections = Freq,
            id = Var1
        )

    ###
    no_referenced_works <- sapply(
        snowball$node["referenced_works"][[1]],
        length
    )

    no_referenced_works <- data.frame(
        id = snowball$node["id"],
        no_referenced_works = no_referenced_works
    )

    ###
    citations_per_year <- flat_snow |>
        select(
            id,
            publication_year,
            cited_by_count
        ) |>
        mutate(
            years_published = 2023 - publication_year
        ) |>
        mutate(
            avg_citations_per_year = cited_by_count / years_published
        ) |>
        arrange(desc(cited_by_count)) |>
        select(
            id,
            avg_citations_per_year
        ) |>
        rename(cited_global_per_year = avg_citations_per_year)

    ###

    export <- flat_snow |>
        select(
            id,
            author,
            publication_year,
            display_name,
            doi,
            so,
            cited_by_count,
            ab
        ) |>
        rename(
            cited_global = cited_by_count,
            title = display_name,
            abstract = ab,
            journal = so,
            year = publication_year
        )

    export$author_institute <- sapply(
        flat_snow$author,
        function(z) {
            paste(unlist(z["institution_display_name"]), collapse = ", ")
        }
    )


    export$institute_country <- sapply(
        flat_snow$author,
        function(z) {
            paste(unlist(z["institution_country_code"]), collapse = ", ")
        }
    )


    result <- export |>
        full_join(no_edges, by = "id") |>
        full_join(no_referenced_works, by = "id") |>
        full_join(citations_per_year, by = "id") |>
        dplyr::relocate(author, .after = id) |>
        arrange(desc(cited_global))

    la <- sapply(
        result$abstract,
        function(a) {
            isTRUE(nchar(a) >= 3000)
        }
    )


    result$abstract[la] <- substr(result$abstract[la], 1, 3000)

    result <- result[
        c(
            "id",
            "doi",
            "author",
            "year",
            "title",
            "journal",
            "abstract",
            "cited_global",
            "cited_global_per_year",
            "no_referenced_works",
            "no_connections",
            "author_institute",
            "institute_country"
        )
    ]

    return(result)
}
