#' Plot an Interactive Citation Network from a Snowball Search
#'
#' This function creates a interactive snowball seaarch network using the networkD3 package.
#'
#' @param snowball The snowball object containing the network data. The object is returned from the
#'   \link[openalexR]{oa_snowball function in the `openalexR`` package
#'
#' @importFrom networkD3 forceNetwork
#'
#' @return A networkD3 object representing the interactive network plot.
#'
#' @md
#'
#' @examples
#' \dontrun{
#' plot_snowball_interactive(snowball)
#' }
#' @export
plot_snowball_interactive <- function(snowball) {
    ## Simple forceNetwork
    networkData <- data.frame(
        src = snowball_1$edges$from,
        target = snowball_1$edges$to,
        stringsAsFactors = FALSE
    )

    nodes <- data.frame(
        name = snowball_1$nodes$id,
        author = IPBES.R::abbreviate_authors(snowball_1$nodes),
        doi = snowball_1$nodes$doi,
        nodesize = snowball_1$nodes$cited_by_count / (2024 - snowball_1$nodes$publication_year) * 0.5,
        stringsAsFactors = FALSE
    )
    nodes$id <- 0:(nrow(nodes) - 1)

    # create a data frame of the edges that uses id 0:9 instead of their names
    edges <- networkData |>
        left_join(nodes, by = c("src" = "name")) |>
        select(-src, -author) |>
        rename(source = id) |>
        left_join(nodes, by = c("target" = "name")) |>
        select(-target, -author) |>
        rename(target = id) |>
        mutate(width = 1)

    # make a grouping variable that will match to colours
    nodes$group <- ifelse(
        nodes$name %in% gsub("^https://openalex.org/", "", key_works$id),
        "key_paper",
        "other"
    )
    nodes$oa_id <- nodes$name
    nodes$name <- nodes$author

    # control colours with a JS ordinal scale
    ColourScale <- 'd3.scaleOrdinal()
                        .domain(["key_paper", "other"])
                     .range(["#FF6900", "#694489"]);'

    openDOI <- "window.open(d.doi)"

    MyClickScript <- 'alert("You clicked " + d.name + " with the doi " +
             d.doi +  " of your original R data frame");'

    nwg <- forceNetwork(
        Links = edges,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        NodeID = "name",
        Nodesize = "nodesize",
        Group = "group",
        Value = "width",
        opacity = 0.9,
        zoom = TRUE,
        colourScale = JS(ColourScale),
        fontSize = 20,
        legend = TRUE,
        clickAction = openDOI
    )

    nwg$x$nodes$doi <- nodes$doi

    nwg
}
