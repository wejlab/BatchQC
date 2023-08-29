
#' Process Dendrogram
#'
#' This function processes count data for dendrogram plotting
#' @param se SummarizedExperiment object
#' @param assay assay to plot
#' @param batch_var sample metadata column
#' @import tibble
#' @import ggdendro
#' @import dplyr
#' @return named list of dendrogram data
#' @return dendrogram_segments is data representing segments of the dendrogram
#' @return dendrogram_ends is data representing ends of the dendrogram
#'
#' @export
process_dendrogram <- function(se, assay, batch_var) {

    data <- t(assays(se)[[assay]])
    dat <- as.data.frame(data) %>%
        mutate(sample_name = paste("sample", seq_len(nrow(data)), sep = "_"))
    rownames(dat) <- dat$sample_name
    sample_name <- dat$sample_name
    metadata <- cbind(as.data.frame(colData(se)), sample_name)
    metadata[] <- lapply(metadata, as.character)
    dist_matrix <- stats::dist(dat, method = "euclidean")

    dendrogram <- stats::as.dendrogram(
        stats::hclust(
            dist_matrix,
            method = "complete")
    )

    dendrogram_data <- dendro_data(dendrogram)
    dendrogram_segments <- dendrogram_data$segments
    dendrogram_ends <- dendrogram_segments %>%
        filter(yend == 0) %>%
        left_join(dendrogram_data$labels, by = "x") %>%
        rename(sample_name = label) %>%
        left_join(metadata, by = "sample_name")

    return(list(dendrogram_ends = dendrogram_ends,
        dendrogram_segments = dendrogram_segments))

}

#' Dendrogram Plot
#'
#' This function creates a dendrogram plot
#' @param se SummarizedExperiment object
#' @param assay assay to plot
#' @param batch_var sample metadata column representing batch
#' @param category_var sample metadata column representing category of interest
#' @import ggdendro
#' @import RColorBrewer
#' @import dplyr
#' @return named list of dendrogram plots
#' @return dendrogram is a dendrogram ggplot
#' @return circular_dendrogram is a circular dendrogram ggplot
#'
#' @export
dendrogram_plotter <- function(se, assay, batch_var, category_var) {
    # Need to add in an if statement
    # if(batch_var == "batch_var"){
    # rename the "batch_var" column to "batch"
    #}

    dends <- process_dendrogram(se, assay, batch_var)

    dendrogram_ends <- dends$dendrogram_ends

    dendrogram_segments <- dends$dendrogram_segments

    unique_vars <- levels(factor(dendrogram_ends[, batch_var])) %>%
        as.data.frame() %>%
        rownames_to_column("row_id")

    color_count <- length(unique(unique_vars$.))
    get_palette <- grDevices::colorRampPalette(brewer.pal(
        n = length(unique(dendrogram_ends[, batch_var])),
        name = "Paired"))
    palette <- get_palette(color_count) %>% as.data.frame() %>%
        rename("color" = ".") %>%
        rownames_to_column(var = "row_id")
    color_list <- left_join(unique_vars, palette, by = "row_id") %>%
        select(-row_id)
    annotation_color <- as.character(color_list$color)
    names(annotation_color) <- color_list$.

    dendrogram <- ggplot() +
        geom_segment(data = dendrogram_segments,
            aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_segment(data = dendrogram_ends,
            aes(x = x, y = y.x, xend = xend, yend = yend,
                color = dendrogram_ends[, batch_var])) +
        scale_color_manual(values = annotation_color,
            limits = names(annotation_color),
            name = as.character(batch_var)) +
        scale_y_reverse() +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        theme_bw() + ylab("Distance")

    circular_dendrogram <- dendrogram + coord_polar(theta = "x")

    return(list(dendrogram = dendrogram,
        circular_dendrogram = circular_dendrogram))
}
