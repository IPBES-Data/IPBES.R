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
#' @param select_fields A character vector of fields to be selected from the downloaded data. The fields
#'  included in the default are
#'  `id`, `doi`, `authorships`, `publication_year`, `display_name`, `abstract_inverted_index`, and `topics`.
#'  These are needed to use the function `corpuus_pages_to_arrow()`.
#' @param verbose Logical indicating whether to display progress messages. Default is TRUE.
#' @param dry_run Logical indicating whether to run the function without downloading any data. Default is FALSE.
#' @param mc_cores The number of cores to be used for parallel processing. Default is 3.
#'   This is limiting the number of parallel downloads
#' @param ... Additional filter arguments.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_corpus(title_and_abstract_search = "climate change")
#' }
corpus_download <- function(
    pages_dir = file.path(".", "data", "pages"),
    title_and_abstract_search,
    continue = TRUE,
    delete_pages_dir = FALSE,
    select_fields = c(
      "id",
      "doi",
      "authorships",
      "publication_year",
      "display_name",
      "abstract_inverted_index",
      "topics"
    ),
    verbose = TRUE,
    dry_run = FALSE,
    mc_cores = 3,
    ...
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
    verbose = FALSE,
    ...
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
          message("\nGetting data for year ", y, " ...")
        }

        output_path <- file.path(pages_dir, paste0("set_publication_year=", y))

        dir.create(
          path = output_path,
          showWarnings = FALSE,
          recursive = TRUE
        )

        file.create(file.path(output_path, "00_in_progress_00"))

        openalexR::oa_query(
          title_and_abstract.search = compact(title_and_abstract_search),
          publication_year = y,
          options = list(
            select = select_fields
          ),
          ...
        ) |>
          IPBES.R:::oa_request(
            output_pages_to = output_path,
            pages_save_function = saveRDS,
            verbose = verbose
          )

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
