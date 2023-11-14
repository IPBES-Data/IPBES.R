#' Convert Snowball to XLSX
#'
#' This function takes a snowball object and a filename, and converts the snowball object to an XLSX file.
#'
#' @param snowball A snowball object containing the data to be converted.
#' @param xls_filename The name of the XLSX file to be created.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @importFrom openalexR snowball2df
#' @importFrom dplyr select mutate arrange rename full_join relocate filter desc
#' @importFrom writexl write_xlsx
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' to_xlsx(snowball, "example.xlsx")
#' }
to_xlsx <- function(snowball, xls_filename) {
    flat_snowball <- openalexR::snowball2df(snowball)

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
            publication_year,
            display_name,
            doi,
            cited_by_count,
            ab
        ) |>
        rename(
            cited_global = cited_by_count,
            title = display_name,
            abstract = ab
        )


    export$author <- sapply(
        flat_snow$author,
        function(z) {
            paste(unlist(z["au_display_name"]), collapse = ", ")
        }
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


    concepts <- lapply(
        flat_snow$concepts,
        function(x) {
            x |>
                select(level, display_name, score)
        }
    )

    export$concepts_l0 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 0) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )

    export$concepts_l1 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 1) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )

    export$concepts_l2 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 2) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )

    export$concepts_l3 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 3) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )

    export$concepts_l4 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 4) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )

    export$concepts_l5 <- sapply(
        concepts,
        function(x) {
            x |>
                filter(level == 5) |>
                mutate(name = paste0(display_name, " (", round(score, digits = 3), ")")) |>
                select(name) |>
                unlist() |>
                paste0(collapse = ", ")
        }
    )


    xlsx <- export |>
        full_join(no_edges, by = "id") |>
        full_join(no_referenced_works, by = "id") |>
        full_join(citations_per_year, by = "id") |>
        dplyr::relocate(author, .after = id) |>
        arrange(desc(cited_global))

    la <- sapply(
        xlsx$abstract,
        function(a) {
            isTRUE(nchar(a) >= 3000)
        }
    )


    xlsx$abstract[la] <- substr(xlsx$abstract[la], 1, 3000)

    xlsx |>
        dplyr::relocate(cited_global_per_year, .after = cited_global) |>
        dplyr::relocate(no_referenced_works, .after = doi) |>
        dplyr::relocate(no_connections, .before = abstract) |>
        dplyr::relocate(concepts_l0, .before = abstract) |>
        dplyr::relocate(concepts_l1, .before = abstract) |>
        dplyr::relocate(concepts_l2, .before = abstract) |>
        dplyr::relocate(concepts_l3, .before = abstract) |>
        dplyr::relocate(concepts_l4, .before = abstract) |>
        dplyr::relocate(concepts_l5, .before = abstract) |>
        dplyr::relocate(author_institute, .before = abstract) |>
        dplyr::relocate(institute_country, .before = abstract) |>
        writexl::write_xlsx(xls_filename)
}
