#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param snowball PARAM_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param path PARAM_DESCRIPTION, Default: 'figures'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[tidygraph]{as_tbl_graph.data.frame}}
#'  \code{\link[ggraph]{ggraph}}, \code{\link[ggraph]{geom_edge_link}}, \code{\link[ggraph]{geom_node_point}}, \code{\link[ggraph]{geom_node_text}}, \code{\link[ggraph]{theme_graph}}
#'  \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{character(0)}}, \code{\link[ggplot2]{scale_size}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{ggsave}}, \code{\link[ggplot2]{margin}}, \code{\link[ggplot2]{c("guide_bins", "guide_colourbar", "guide_coloursteps", "guide_legend", "guides", "guides")}}, \code{\link[ggplot2]{labs}}
#' @rdname plot_snowball
#' @export
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_label theme_graph scale_edge_width
#' @importFrom ggplot2 aes scale_size scale_fill_manual theme ggsave element_rect guides ggtitle
plot_snowball <- function(snowball, name, path = "figures") {
    snowball$nodes$cited_by_count_by_year <- snowball$nodes$cited_by_count / (2024 - snowball$nodes$publication_year)

    ### Size cited_by_count_by_year
    p_cby <- snowball |>
        tidygraph::as_tbl_graph() |>
        ggraph::ggraph(graph = , layout = "stress") +
        ggraph::geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
        ggraph::geom_node_point(ggplot2::aes(fill = oa_input, size = cited_by_count_by_year), shape = 21, color = "white") +
        ggraph::geom_node_label(ggplot2::aes(filter = oa_input, label = id), nudge_y = 0.2, size = 3) +
        ggraph::scale_edge_width(range = c(0.1, 1.5), guide = "none") +
        ggplot2::scale_size(range = c(3, 10), guide = "none") +
        ggplot2::scale_fill_manual(values = c("#a3ad62", "#d46780"), na.value = "grey", name = "") +
        ggraph::theme_graph() +
        ggplot2::theme(
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.position = "bottom"
        ) +
        ggplot2::guides(fill = "none") +
        ggplot2::ggtitle(paste0(name, " Cited by average count per year"))

    ggplot2::ggsave(file.path(path, paste0(name, "_cited_by_count_by_year.pdf")), plot = p_cby, device = cairo_pdf, width = 20, height = 15)
    ggplot2::ggsave(file.path(path, paste0(name, "_cited_by_count_by_year.png")), plot = p_cby, width = 20, height = 15, bg = "white")

    ### Size cited_by_count
    p_cb <- snowball |>
        tidygraph::as_tbl_graph() |>
        ggraph::ggraph(graph = , layout = "stress") +
        ggraph::geom_edge_link(ggplot2::aes(alpha = after_stat(index)), show.legend = FALSE) +
        ggraph::geom_node_point(ggplot2::aes(fill = oa_input, size = cited_by_count), shape = 21, color = "white") +
        ggraph::geom_node_label(ggplot2::aes(filter = oa_input, label = id), nudge_y = 0.2, size = 3) +
        ggraph::scale_edge_width(range = c(0.1, 1.5), guide = "none") +
        ggplot2::scale_size(range = c(3, 10), guide = "none") +
        ggplot2::scale_fill_manual(values = c("#a3ad62", "#d46780"), na.value = "grey", name = "") +
        ggraph::theme_graph() +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            legend.position = "bottom"
        ) +
        ggplot2::guides(fill = "none") +
        ggplot2::ggtitle(paste0(name, " Cited by count"))

    ggplot2::ggsave(file.path(path, paste0(name, "_cited_by_count.pdf")), plot = p_cb, device = cairo_pdf, width = 20, height = 15)
    ggplot2::ggsave(file.path(path, paste0(name, "_cited_by_count.png")), plot = p_cb, width = 20, height = 15, bg = "white")

    return(list(size_by_count = p_cb, size_by_count_per_year = p_cby))
}
