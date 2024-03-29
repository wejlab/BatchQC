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
#' @examples
#' library(scran)
#' se <- mockSCE()
#' process_dendro <- BatchQC::process_dendrogram(se, "counts")
#' dendrogram_ends <- process_dendro$dendrogram_ends
#' col <- process_dendro$condition_var
#' dendro_colors <- dendrogram_color_palette(col = "Treatment",
#'                                     dendrogram_info = dendrogram_ends)
#' dendro_colors
#'
#' @export

dendrogram_color_palette <- function(col, dendrogram_info) {

    # Create unique_vars dataframe
    unique_vars <- unique(dendrogram_info[, col]) %>%
        as.data.frame() %>% rownames_to_column("row_id")

    # Determine color count and palette
    color_count <- length(unique_vars$.)

    if (color_count < 3) {
        color_list <- c("Red", "Blue", "Green", "Yellow")
    } else {
        color_list <- color_palette(n = color_count)
    }

    palette <- color_list %>%
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
