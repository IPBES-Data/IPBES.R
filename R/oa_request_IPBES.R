#' Get bibliographic records from OpenAlex database
#'
#' This a slight adaptation from the function `oa_request` from the package [openalexR](https://github.com/ropensci/openalexR)
#' It has the additional argument `output_path` to save the results in a file and not compile them in memory.
#' When the transfer is interrupted, the existing files are not overwritten but skipped.
#' Here the original documentation:
#' `oa_request` makes a request and downloads bibliographic records from
#' OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
#' The function \code{oa_request} queries OpenAlex database using a query
#' formulated through the function \code{oa_query}.
#'
#' @param query_url Character string.
#' A search query formulated using the OpenAlex API language and
#' can be generated with \code{oa_query}.
#' @param per_page Numeric. Number of items to download per page.
#' The per-page argument can assume any number between 1 and 200.
#' Defaults to 200.
#' @param paging Character.
#' Either "cursor" for cursor paging or "page" for basic paging.
#' When used with `options$sample` and or `pages`,
#' paging is also automatically set to basic paging: `paging = "page"`
#' to avoid duplicates and get the right page.
#' See https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/paging.
#' @param pages Integer vector.
#' The range of pages to return. If NULL, return all pages.
#' @param count_only Logical.
#' If TRUE, the function returns only the number of item matching the query.
#' Defaults to FALSE.
#' @param mailto Character string.
#' Gives OpenAlex an email to enter the polite pool.
#' @param api_key Character string.
#' Your OpenAlex Premium API key, if available.
#' @param verbose Logical.
#' If TRUE, print information about the querying process. Defaults to TRUE.
#' @param output_path Character string.
#' If `NULL` (default), the results are compiled in memory as in the original function.
#' If a character string, the results are saved in files named `page_PAGENO.rds` in the output path `output_path`.
#' @return a data.frame or a list of bibliographic records.
#'
#' For more extensive information about OpenAlex API, please visit:
#' <https://docs.openalex.org>
#'
#' @md
#'
#' @examples
#' \dontrun{
#'
#' ### EXAMPLE 1: Full record about an entity.
#'
#' # Query to obtain all information about a particular work/author/institution/etc.:
#'
#' #  The following paper is associated to the OpenAlex-id W2755950973.
#'
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#'
#' query_work <- oa_query(
#'     identifier = "W2755950973",
#'     entity = "works",
#'     endpoint = "https://api.openalex.org"
#' )
#'
#' res <- oa_request(
#'     query_url = query_work,
#'     count_only = FALSE,
#'     verbose = FALSE
#' )
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A5069892096.
#'
#'
#' query_author <- oa_query(
#'     identifier = "A5069892096",
#'     entity = "authors",
#'     endpoint = "https://api.openalex.org"
#' )
#'
#' res <- oa_request(
#'     query_url = query_author,
#'     count_only = FALSE,
#'     verbose = FALSE
#' )
#'
#'
#'
#' ### EXAMPLE 2: all works citing a particular work.
#'
#' # Query to search all works citing the article:
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' #  published in 2021.
#' #  The paper is associated to the OpenAlex id W2755950973.
#'
#' #  Results have to be sorted by relevance score in a descending order.
#'
#' query2 <- oa_query(
#'     identifier = NULL,
#'     entity = "works",
#'     filter = "cites:W2755950973",
#'     from_publication_date = "2021-01-01",
#'     to_publication_date = "2021-12-31",
#'     search = NULL,
#'     endpoint = "https://api.openalex.org"
#' )
#'
#' res2 <- oa_request(
#'     query_url = query2,
#'     count_only = FALSE,
#'     verbose = FALSE
#' )
#'
#' ### EXAMPLE 3: All works matching a string in their title
#'
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Results have to be sorted by relevance score in a descending order.
#'
#'
#' query3 <- oa_query(
#'     identifier = NULL,
#'     entity = "works",
#'     filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'     from_publication_date = "2020-01-01",
#'     to_publication_date = "2021-12-31",
#'     search = NULL,
#'     endpoint = "https://api.openalex.org"
#' )
#'
#' res3 <- oa_request(
#'     query_url = query3,
#'     count_only = FALSE,
#'     verbose = FALSE
#' )
#'
#' ### EXAMPLE 4: How to check how many works match a query
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Query only to know how many works could be retrieved (count_only=TRUE)
#'
#' query4 <- oa_query(
#'     identifier = NULL,
#'     entity = "works",
#'     filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'     from_publication_date = "2020-01-01",
#'     to_publication_date = "2021-12-31",
#'     search = NULL,
#'     endpoint = "https://api.openalex.org"
#' )
#'
#' res4 <- oa_request(
#'     query_url = query4,
#'     count_only = TRUE,
#'     verbose = FALSE
#' )
#'
#' res4$count # number of items retrieved by our query
#' }
#' @export
#'
oa_request_IPBES <- function(query_url,
                             per_page = 200,
                             paging = "cursor",
                             pages = NULL,
                             count_only = FALSE,
                             mailto = openalexR:::oa_email(),
                             api_key = openalexR:::oa_apikey(),
                             verbose = FALSE,
                             output_path = NULL) {
    # https://httr.r-lib.org/articles/api-packages.html#set-a-user-agent
    ua <- httr::user_agent("https://github.com/ropensci/openalexR/")

    # building query...
    # first, download info about n. of items returned by the query
    is_group_by <- grepl("group_by", query_url)
    query_ls <- if (is_group_by) list() else list("per-page" = 1)

    if (!is.null(mailto)) {
        if (openalexR:::isValidEmail(mailto)) {
            query_ls[["mailto"]] <- mailto
        } else {
            message(mailto, " is not a valid email address")
        }
    }

    res <- openalexR:::api_request(query_url, ua, query = query_ls, api_key = api_key)

    if (is_group_by) {
        return(res$group_by)
    }

    if (!is.null(res$meta)) {
        ## return only item counting
        if (count_only) {
            return(res$meta)
        }
    } else {
        return(res)
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

    # Setting items per page
    query_ls[["per-page"]] <- per_page

    # Activation of cursor pagination
    data <- vector("list", length = n_pages)
    res <- NULL
    for (i in pages) {
        if (verbose) pb$tick()
        Sys.sleep(1 / 100)
        next_page <- openalexR:::get_next_page(paging, i, res)
        query_ls[[paging]] <- next_page
        res <- openalexR:::api_request(query_url, ua, query = query_ls)
        if (is.null(output_path)) {
            if (!is.null(res$results)) data[[i]] <- res$results
        } else {
            fn <- file.path(output_path, paste0("page_", i, ".rds"))
            if (!file.exists(fn)) {
                saveRDS(res, fn)
            }
        }
    }

    if (is.null(output_path)) {
        return(unlist(data, recursive = FALSE))
    } else {
        return(output_path)
    }
}
