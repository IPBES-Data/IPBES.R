#' Create a DataTable with various export options
#'
#' This is a wrapper around the DT::datatable() function that adds buttons for exporting the table to CSV, Excel, and PDF, as well as a print button. It also enables scrolling and fixed columns by default.
#' @param data The data to be displayed in the DataTable
#' @param fn The filename to be used for the exported files when using the default button definition
#' @param buttons A list of buttons to be displayed in the DataTable
#' @param scroller Logical value indicating whether to enable scrolling in the DataTable
#' @param scrollY JavaScript code to set the height of the DataTable
#' @param scrollX Logical value indicating whether to enable horizontal scrolling in the DataTable
#' @param fixedColumns A list specifying the number of fixed columns in the DataTable
#' @param escape Logical value indicating whether to escape HTML entities in the DataTable
#' @param ... Additional options to be passed to the DT::datatable function
#'
#' @importFrom DT datatable JS
#'
#' @return A DataTable object
#'
#' @export
#'
#' @examples
#' table_dt(iris)
table_dt <- function(
    data,
    fn = "datatable",
    buttons = list(
        list(
            extend = "csv",
            filename = fn
        ),
        list(
            extend = "excel",
            filename = fn
        ),
        list(
            extend = "pdf",
            filename = fn,
            orientation = "landscape",
            customize = DT::JS(
                "function(doc) {",
                "  doc.defaultStyle.fontSize = 5;", # Change the font size
                "}"
            )
        ),
        "print"
    ),
    scroller = TRUE,
    scrollY = JS("window.innerHeight * 0.7 + 'px'"),
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 4),
    escape = FALSE,
    ...) {
    DT::datatable(
        data = data,
        extensions = c(
            "Buttons",
            "FixedColumns",
            "Scroller"
        ),
        options = list(
            dom = "Bfrtip",
            buttons = buttons,
            scroller = scroller,
            scrollY = scrollY,
            scrollX = scrollX,
            fixedColumns = fixedColumns,
            ...
        ),
        escape = escape
    )
}
