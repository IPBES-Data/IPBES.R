#' Convert Downloaded Corpus Data to Arrow Format
#'
#' This function converts downloaded data in RDS format to Arrow format.
#'
#' @param pages_dir The directory where the downloaded data is stored.
#' @param arrow_dir The directory where the converted Arrow data will be stored.
#' @param continue Logical indicating whether to continue from where the conversion was left off.
#' @param delete_arrow_dir Logical indicating whether to delete the existing Arrow directory before conversion.
#' @param verbose Logical indicating whether to display messages.
#' @param dry_run Logical indicating whether to run the function without converting any data. Default is FALSE.
#' @param mc_cores The number of cores to be used for parallel processing. Default is 3.
#'   This is limited by memory requirements.
#'
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom openalexR works2df
#' @importFrom arrow write_dataset
#'
#' @export
corpus_pages_to_arrow <- function(
    pages_dir = file.path(".", "data", "pages"),
    arrow_dir = file.path(".", "data", "corpus"),
    continue = TRUE,
    delete_arrow_dir = FALSE,
    verbose = FALSE,
    dry_run = FALSE,
    mc_cores = 3
    #
    ) {
  if (!dir.exists(pages_dir)) {
    stop("The pages directory does not exist.")
  }

  if (delete_arrow_dir) {
    unlink(
      arrow_dir,
      recursive = TRUE
    )
  }

  if (!dir.exists(arrow_dir)) {
    dir.create(
      path = arrow_dir,
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  years <- list.dirs(
    path = pages_dir,
    full.names = TRUE,
    recursive = FALSE
  )


  if (continue) {
    years_done <- list.dirs(
      path = arrow_dir,
      full.names = TRUE,
      recursive = FALSE
    )

    years <- years[
      !(
        gsub(
          x = years,
          pattern = paste0("^", pages_dir, "/set_publication_year="),
          replacement = ""
        ) %in% gsub(
          x = years_done,
          pattern = paste0("^", arrow_dir, "/publication_year="),
          replacement = ""
        )
      )
    ]
  }

  if (length(years) == 0) {
    message("All years have been processed.")
    return(invisible())
  }

  if (!dry_run) {
    pbapply::pblapply(
      years,
      function(year) {
        if (verbose) {
          message("\n     Processing year ", year, " ...\n")
        }
        sets <- list.files(
          path = year,
          pattern = "^set_",
          full.names = TRUE,
          recursive = TRUE
        )
        data <- parallel::mclapply(
          sets,
          function(set) {
            data <- readRDS(file.path(set))
            # Fix download of `openalexR::works2df()` unsupported `title` field
            for (i in 1:length(data)) {
              names(data[[i]])[names(data[[i]]) == "title"] <- "display_name"
            }
            data <- openalexR::works2df(data, verbose = FALSE)

            data$author_abbr <- IPBES.R::abbreviate_authors(data)

            data$set <- set |>
              basename() |>
              gsub(pattern = "^set_", replacement = "") |>
              gsub(pattern = ".rds$", replacement = "")

            ### Data cleaning

            if (is.null(data$doi)) {
              data$doi <- as.character(NA)
            } else {
              data$doi <- as.character(data$doi)
            }
            #
            data$publication_year <- as.integer(data$publication_year)
            if (is.null(data$ab)) {
              data$ab <- as.character(NA)
            }
            #
            if (is.null(data$id)) {
              data$id <- as.character(NA)
            } else {
              data$id <- as.character(data$id)
            }
            #
            if (is.null(data$display_name)) {
              data$display_name <- as.character(NA)
            } else {
              data$display_name <- as.character(data$display_name)
            }
            #
            if (is.null(data$ab)) {
              data$ab <- as.character(NA)
            } else {
              data$ab <- as.character(data$ab)
            }
            #
            if (is.null(data$author_abbr)) {
              data$author_abbr <- as.character(NA)
            } else {
              data$author_abbr <- as.character(data$author_abbr)
            }
            #
            #
            #

            ####
            ####  Templates definition
            ####

            author_template <- data.frame(
              au_id = as.character(NA),
              au_display_name = as.character(NA),
              au_orcid = as.character(NA),
              author_position = as.character(NA),
              is_corresponding = as.logical(NA),
              au_affiliation_raw = as.character(NA),
              institution_id = as.character(NA),
              institution_display_name = as.character(NA),
              institution_ror = as.character(NA),
              institution_country_code = as.character(NA),
              institution_type = as.character(NA),
              institution_lineage = as.character(NA)
            )
            #
            topics_template <- data.frame(
              i = as.integer(NA),
              score = as.numeric(NA),
              name = as.character(NA),
              id = as.character(NA),
              display_name = as.character(NA)
            )
            ####
            #### authors
            ####

            i_a <- sapply(data$author, is.logical)
            data$author[i_a] <- list(author_template)

            data$author[!i_a] <- lapply(
              data$author[!i_a],
              function(x) {
                if (is.null(x$au_affiliation_raw)) {
                  x$au_affiliation_raw <- as.character(NA)
                }
                x |>
                  dplyr::mutate(
                    au_id = as.character(au_id),
                    au_display_name = as.character(au_display_name),
                    au_orcid = as.character(au_orcid),
                    author_position = as.character(author_position),
                    is_corresponding = as.logical(is_corresponding),
                    au_affiliation_raw = as.character(au_affiliation_raw),
                    institution_id = as.character(institution_id),
                    institution_display_name = as.character(institution_display_name),
                    institution_ror = as.character(institution_ror),
                    institution_country_code = as.character(institution_country_code),
                    institution_type = as.character(institution_type),
                    institution_lineage = as.character(institution_lineage)
                  )
              }
            )

            ####
            #### topics
            ####

            i_t <- sapply(
              data$topics,
              function(x) {
                is.logical(x) | length(x) == 0
              }
            )
            data$topics[i_t] <- list(topics_template)

            data$topics[!i_t] <- lapply(
              data$topics[!i_t],
              function(x) {
                x |>
                  dplyr::mutate(
                    i = as.integer(i),
                    score = as.numeric(score),
                    dplyr::across(
                      c(
                        name,
                        id,
                        display_name
                      ),
                      as.character
                    )
                  )
              }
            )

            # data <- serialize_arrow(data)

            data |>
              arrow::write_dataset(
                path = arrow_dir,
                partitioning = c("publication_year", "set"),
                format = "parquet",
                existing_data_behavior = "overwrite"
              )
          },
          mc.cores = mc_cores
        )
      }
    ) |>
      invisible()
  }


  pages <- list.dirs(
    path = pages_dir,
    full.names = TRUE,
    recursive = FALSE
  ) |>
    gsub(
      x = years,
      pattern = paste0("^", pages_dir, "/set_publication_year="),
      replacement = ""
    )

  arrow <- list.dirs(
    path = arrow_dir,
    full.names = TRUE,
    recursive = FALSE
  ) |>
    gsub(
      x = years_done,
      pattern = paste0("^", arrow_dir, "/publication_year="),
      replacement = ""
    )



  not_completed <- pages[!(pages %in% arrow)]

  if (length(not_completed) > 0) {
    warning(
      "The following years did not complete successful:\n",
      paste0(not_completed, collapse = "\n"),
      "\nPlease run the function again with the same parameters but\n",
      "'delete_arrow_dir = FALSE, continue = TRUE'"
    )
  } else {
    message("All years have been processed.")
  }

  return(length(not_completed))
}
