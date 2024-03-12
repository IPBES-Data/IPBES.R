#' Download Corpus
#'
#' This function downloads a corpus of documents based on a given title and abstract search query.
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom openalexR oa_fetch oa_query
#'
#' @param pages_dir The directory where the downloaded pages will be stored. Default is "./data/pages".
#' @param title_and_abstract_search The search query for the title and abstract of the documents to be downloaded.
#' @param continue Logical indicating whether to continue downloading from where it left off. Default is TRUE.
#' @param delete_pages_dir Logical indicating whether to delete the pages directory before downloading.
#'   Default is FALSE.
#' @param verbose Logical indicating whether to display progress messages. Default is TRUE.
#' @param mc_cores The number of cores to be used for parallel processing. Default is 3.
#'   This is limiting the number of parallel downloads
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_corpus(title_and_abstract_search = "climate change")
#' }
download_corpus <- function(
    pages_dir = file.path(".", "data", "pages"),
    title_and_abstract_search,
    continue = TRUE,
    delete_pages_dir = FALSE,
    verbose = TRUE,
    mc_cores = 3
    #
    ) {
    if (delete_pages_dir) {
        unlink(
            pages_dir,
            recursive = TRUE
        )
    }

    dir.create(
        path = pages_dir,
        showWarnings = FALSE,
        recursive = TRUE
    )

    years <- oa_fetch(
        title_and_abstract.search = compact(title_and_abstract_search),
        group_by = "publication_year",
        paging = "cursor",
        verbose = FALSE
    )$key

    if (continue) {
        processed <- list.dirs(
            path = pages_dir,
            full.names = FALSE,
            recursive = FALSE
        ) |>
            gsub(
                pattern = paste0("^pages_publication_year=", ""),
                replacement = ""
            )

        interrupted <- list.files(
            path = pages_dir,
            pattern = "^next_page.rds",
            full.names = TRUE,
            recursive = TRUE
        ) |>
            gsub(
                pattern = paste0("^", pages_dir, "/pages_publication_year=", ""),
                replacement = ""
            ) |>
            gsub(
                pattern = "/next_page.rds$",
                replacement = ""
            )

        completed <- processed[!(processed %in% interrupted)]

        years <- years[!(years %in% completed)]
    }

    pbmcapply::pbmclapply(
        sample(years),
        function(y) {
            if (verbose) {
                message("Getting data for year ", y, " ...")
            }

            output_path <- file.path(pages_dir, paste0("pages_publication_year=", y))

            dir.create(
                path = output_path,
                showWarnings = FALSE,
                recursive = TRUE
            )

            invisible(
                openalexR::oa_query(
                    title_and_abstract.search = compact(title_and_abstract_search),
                    publication_year = y,
                    options = list(
                        select = c(
                            "id",
                            "doi",
                            "authorships",
                            "publication_year",
                            "title",
                            "abstract_inverted_index", "
                            topics"
                        )
                    ),
                    verbose = FALSE
                ) |>
                    IPBES.R::oa_request_IPBES(
                        count_only = FALSE,
                        output_path = output_path,
                        verbose = TRUE
                    )
            )
        },
        mc.cores = mc_cores,
        mc.preschedule = FALSE
    )
}
