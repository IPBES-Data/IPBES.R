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
#' @param set_size The number of works to be downloaded in each set. Default is 1000.
#' @param verbose Logical indicating whether to display progress messages. Default is TRUE.
#' @param dry_run Logical indicating whether to run the function without downloading any data. Default is FALSE.
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
corpus_download_set <- function(
    pages_dir = file.path(".", "data", "pages"),
    title_and_abstract_search,
    continue = TRUE,
    delete_pages_dir = FALSE,
    set_size = 1000,
    verbose = TRUE,
    dry_run = FALSE,
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
                pattern = paste0("^set_publication_year=", ""),
                replacement = ""
            )

        interrupted <- list.files(
            path = pages_dir,
            pattern = "00_in_progress_00",
            full.names = TRUE,
            recursive = TRUE
        ) |>
            gsub(
                pattern = paste0("^", pages_dir, "/set_publication_year=", ""),
                replacement = ""
            ) |>
            gsub(
                pattern = "/00_in_progress_00",
                replacement = ""
            )

        completed <- processed[!(processed %in% interrupted)]

        years <- years[!(years %in% completed)]
    }

    if (length(years) == 0) {
        message("All years have been processed.")
        return(invisible())
    }

    if (!dry_run) {
        pbmcapply::pbmclapply(
            years,
            function(y) {
                if (verbose) {
                    message("Getting data for year ", y, " ...")
                }

                output_path <- file.path(pages_dir, paste0("set_publication_year=", y))

                dir.create(
                    path = output_path,
                    showWarnings = FALSE,
                    recursive = TRUE
                )

                oar <- openalexR::oa_query(
                    title_and_abstract.search = compact(title_and_abstract_search),
                    publication_year = y,
                    options = list(
                        select = c(
                            "id",
                            "doi",
                            "authorships",
                            "publication_year",
                            "display_name",
                            "abstract_inverted_index",
                            "topics"
                        )
                    )
                ) |>
                    oa_generate(
                        verbose = verbose
                    )

                # set <- vector("list", set_size)
                set <- NULL
                set_no <- 0

                file.create(file.path(output_path, "00_in_progress_00"))
                coro::loop(
                    for (x in oar) {
                        set <- c(set, list(x))
                        if ((length(set) >= set_size) | isTRUE(x == coro::exhausted())) {
                            saveRDS(set, file.path(output_path, paste0("set_", set_no, ".rds")))
                            # set <- vector("list", set_size) # reset recs
                            set <- list()
                            set_no <- set_no + 1
                        }
                    }
                )
                ### and save the last one
                saveRDS(set, file.path(output_path, paste0("set_", set_no, ".rds")))
                file.create(file.path(output_path, "00_complete_00"))
                file.rename(
                    file.path(output_path, "00_in_progress_00"),
                    file.path(output_path, "00_complete_00")
                )
            },
            mc.cores = mc_cores,
            mc.preschedule = FALSE
        ) |>
            invisible()
    }

    in_progress <- list.files(file.path(pages_dir, "00_in_progress_00"), full.names = TRUE, recursive = TRUE)

    if (length(in_progress) > 0) {
        warning(
            "The following years did not complete successful:\n",
            paste0(in_progress, collapse = "\n"),
            "\nPlease run the function again with the same parameters but\n",
            "'delete_pages_dir = FALSE, continue = TRUE'"
        )
    } else {
        message("All years have been processed.")
    }

    return(length(in_progress))
}
