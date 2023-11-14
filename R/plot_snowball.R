#' Plot Snowball
#'
#' This function takes a snowball object and a name, and creates two plots: one sized by cited_by_count and the other by cited_by_count_by_year.
#' The plots are saved as both PDF and PNG in the specified path.
#'
#' @param snowball A snowball object containing the data to be plotted.
#' @param name The name to be used in the plot titles and file names.
#' @param path The path where the plot files will be saved. Default is "figures".
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_label theme_graph scale_edge_width
#' @importFrom ggplot2 aes after_stat scale_size scale_fill_manual theme element_rect guides ggtitle ggsave
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' plot_snowball(snowball, "example")
#' }
plot_snowball <- function(snowball, name, path = "figures") {
    snowball$nodes$cited_by_count_by_year <- snowball$nodes$cited_by_count / (2024 - snowball$nodes$publication_year)

    ### Size cited_by_count_by_year
    p_cby <- snowball |>
        tidygraph::as_tbl_graph() |>
        ggraph::ggraph(graph = , layout = "stress") +
        ggraph::geom_edge_link(aes(alpha = ggplot2::after_stat(index)), show.legend = FALSE) +
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

    invisible(list(size_by_count = p_cb, size_by_count_per_year = p_cby))
}
