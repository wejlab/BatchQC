#' Dendrogram color palette
#'
#' This function creates the color palette used in the dendrogram plotter
#' @param col string object representing color of the label
#' @param dendrogram_info dendrogram_ends object
#' @import tibble
#' @import ggdendro
#' @import dplyr
#' @import RColorBrewer
#' @return annotation_color vector of colors corresponding to col variable
#'
#' @export

dendrogram_color_palette <- function(col, dendrogram_info) {

    # Create unique_vars dataframe
    unique_vars <- levels(factor(dendrogram_info[, col])) %>%
        as.data.frame() %>% rownames_to_column("row_id")

    # Determine color count and palette
    color_count <- length(unique(unique_vars$.))
    n <- length(unique(dendrogram_info[, col]))
    if (n<5) {
        get_palette <- function(n) {
            hues <- seq(25, 375, length = n + 1)
            grDevices::hcl(h = hues, l = c(40, 65), c = 100)[seq_len(n)]
        }
    } else {
        get_palette <- function(n) {
            hues <- seq(376, 1500, length = n + 1)
            grDevices::hcl(h = hues, l = c(35, 65), c = 100)[seq_len(n)]
        }
    }
    palette <- get_palette(color_count) %>%
        as.data.frame() %>%
        dplyr::rename("color" = ".") %>%
        rownames_to_column(var = "row_id")

    # Join the palette and unique_vars
    color_list <- left_join(unique_vars, palette, by = "row_id") %>%
        select(-row_id)

    # Create a named vector for annotation_color
    annotation_color <- stats::setNames(color_list$color, color_list$.)

    return(annotation_color)
}
