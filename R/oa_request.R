oa_request <- function(
    query_url,
    per_page = 200,
    paging = "cursor",
    pages = NULL,
    output_pages_to = NULL,
    pages_save_function = saveRDS,
    count_only = FALSE,
    mailto = openalexR:::oa_email(),
    api_key = openalexR:::oa_apikey(),
    verbose = FALSE) {
  # https://httr.r-lib.org/articles/api-packages.html#set-a-user-agent
  ua <- httr::user_agent("https://github.com/ropensci/openalexR/")

  # building query...
  is_group_by <- grepl("group_by", query_url)
  if (is_group_by) {
    result_name <- "group_by"
    query_ls <- list()
  } else {
    result_name <- "results"
    query_ls <- list("per-page" = 1)
  }

  if (!is.null(mailto)) {
    if (openalexR:::isValidEmail(mailto)) {
      query_ls[["mailto"]] <- mailto
    } else {
      message(mailto, " is not a valid email address")
    }
  }

  # first, download info about n. of items returned by the query
  res <- openalexR:::api_request(query_url, ua, query = query_ls, api_key = api_key)

  if (!is.null(res$meta)) {
    ## return only item counting
    if (count_only) {
      return(res$meta)
    }
  } else {
    return(res)
  }

  # Setting items per page
  query_ls[["per-page"]] <- per_page

  if (is_group_by) {
    data <- vector("list")
    res <- NULL
    i <- 1
    next_page <- openalexR:::get_next_page("cursor", i, res)
    if (verbose) cat("\nDownloading groups...\n|")
    while (!is.null(next_page)) {
      if (verbose) cat("=")
      Sys.sleep(1 / 100)
      query_ls[[paging]] <- next_page
      res <- openalexR:::api_request(query_url, ua, query = query_ls)
      data <- c(data, res[[result_name]])
      i <- i + 1
      next_page <- openalexR:::get_next_page("cursor", i, res)
    }
    cat("\n")
    return(data)
  }

  n_items <- res$meta$count
  n_pages <- ceiling(n_items / per_page)

  ## number of pages
  if (is.null(pages)) {
    pages <- seq.int(n_pages)
  } else {
    pages <- pages[pages <= n_pages]
    n_pages <- length(pages)
    n_items <- min(n_items - per_page * (utils::tail(pages, 1) - n_pages), per_page * n_pages)
    message("Using basic paging...")
    paging <- "page"
  }

  if (n_items <= 0 || n_pages <= 0) {
    warning("No records found!")
    return(list())
  }

  pg_plural <- if (n_pages > 1) " pages" else " page"

  if (verbose) {
    message(
      "Getting ", n_pages, pg_plural, " of results",
      " with a total of ", n_items, " records..."
    )
    pb <- openalexR:::oa_progress(n = n_pages, text = "OpenAlex downloading")
  }

  pb <- openalexR:::oa_progress(n = n_pages, text = "OpenAlex downloading")

  # Setting items per page
  query_ls[["per-page"]] <- per_page

  # Setup output_pages_to if not NULL
  if (!is.null(output_pages_to)) {
    output_pages_to <- normalizePath(output_pages_to, mustWork = FALSE)
    if (!dir.exists(output_pages_to)) {
      dir.create(output_pages_to)
    }
    result <- character(n_pages)
  }

  # Activation of cursor pagination
  data <- vector("list", length = n_pages)
  res <- NULL
  for (i in pages) {
    if (verbose) pb$tick()
    Sys.sleep(1 / 100)
    next_page <- openalexR:::get_next_page(paging, i, res)
    query_ls[[paging]] <- next_page
    res <- openalexR:::api_request(query_url, ua, query = query_ls)
    next_page <- openalexR:::get_next_page(paging, i + 1, res)
    if (!is.null(output_pages_to)) {
      fn <- file.path(output_pages_to, paste0("set_", i, ".rds"))
      pages_save_function(
        res$results,
        fn
      )
      result[[i]] <- fn
    } else {
      if (!is.null(res$results)) data[[i]] <- res$results
    }
  }

  if (is.null(output_pages_to)) {
    if (grepl("filter", query_url) && grepl("works", query_url)) {
      truncated <- unlist(openalexR:::truncated_authors(data))
      if (length(truncated)) {
        truncated <- openalexR:::shorten_oaid(truncated)
        warning(
          "\nThe following work(s) have truncated lists of authors: ",
          paste(truncated, collapse = ", "),
          ".\nQuery each work separately by its identifier to get full list of authors.\n",
          "For example:\n  ",
          paste0(
            "lapply(c(\"",
            paste(utils::head(truncated, 2), collapse = "\", \""),
            "\"), \\(x) oa_fetch(identifier = x))"
          ),
          "\nDetails at https://docs.openalex.org/api-entities/authors/limitations."
        )
      }
    }
    return(data)
  } else {
    return(result)
  }
}
