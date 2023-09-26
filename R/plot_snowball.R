#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param snowball PARAM_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param path PARAM_DESCRIPTION, Default: 'figures'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_snowball
#' @export 

plot_snowball <- function(snowball, name, path = "figures") {
    snowball$nodes$cited_by_count_by_year <- snowball$nodes$cited_by_count / (2024 - snowball$nodes$publication_year)

    ### Size cited_by_count_by_year
    p_cby <- snowball |>
        as_tbl_graph() |>
        ggraph(graph = , layout = "stress") +
        geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
        geom_node_point(aes(fill = oa_input, size = cited_by_count_by_year), shape = 21, color = "white") +
        geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.2, size = 3) +
        scale_edge_width(range = c(0.1, 1.5), guide = "none") +
        scale_size(range = c(3, 10), guide = "none") +
        scale_fill_manual(values = c("#a3ad62", "#d46780"), na.value = "grey", name = "") +
        theme_graph() +
        theme(
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.position = "bottom"
        ) +
        guides(fill = "none") +
            ggtitle(paste0(name, " Cited by average count per year"))

    ggsave(file.path(path, paste0(name, "_cited_by_count_by_year.pdf")), plot = p_cby, device = cairo_pdf, width = 20, height = 15)
    ggsave(file.path(path, paste0(name, "_cited_by_count_by_year.png")), plot = p_cby, width = 20, height = 15, bg = "white")

    ### Size cited_by_count
    p_cb <- snowball |>
        as_tbl_graph() |>
        ggraph(graph = , layout = "stress") +
        geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
        geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21, color = "white") +
        geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.2, size = 3) +
        scale_edge_width(range = c(0.1, 1.5), guide = "none") +
        scale_size(range = c(3, 10), guide = "none") +
        scale_fill_manual(values = c("#a3ad62", "#d46780"), na.value = "grey", name = "") +
        theme_graph() +
        theme(
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.position = "bottom"
        ) +
        guides(fill = "none") +
            ggtitle(paste0(name, " Cited by count"))

    ggsave(file.path(path, paste0(name, "_cited_by_count.pdf")), plot = p_cb, device = cairo_pdf, width = 20, height = 15)
    ggsave(file.path(path, paste0(name, "_cited_by_count.png")), plot = p_cb, width = 20, height = 15, bg = "white")

    return(p_cb)
}
