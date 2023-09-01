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
    dat <- dat %>% select(-c(sample_name))
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
        dplyr::rename(sample_name = label) %>%
        filter(!is.na(sample_name)) %>%
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
#' @import ggplot2
#' @import dplyr
#' @import ggnewscale
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

    #Color palette
    batch_color <- dendrogram_color_palette(
        col = batch_var, dendrogram_info = dendrogram_ends)
    category_color <- dendrogram_color_palette(
        col = category_var, dendrogram_info = dendrogram_ends)
    geom_label <- dendrogram_alpha_numeric_check(
        dendro_category = dendrogram_ends[, category_var])

    # Create dendrogram plot
    dendrogram <- ggplot() +
        geom_segment(data = dendrogram_segments,
                     aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_segment(data = dendrogram_ends,
                     aes(x = x, y = y.x, xend = xend, yend = yend,
                         color = dendrogram_ends[, batch_var]
                     )) +
        scale_color_manual(values = batch_color, name = batch_var,
                           guide_legend(override.aes = batch_color,
                                        order = 1)) +
        new_scale_color() + # To separate the color palette
        geom_text(data = dendrogram_ends,
                  aes(x = x, y = y.y - 1.5, label = as.character(
                      as.numeric(factor(dendrogram_ends[, category_var]))),
                      color = dendrogram_ends[, category_var]),
                  check_overlap = TRUE, size = 2.2) +
        guides(color = guide_legend(override.aes = list(
            label = "\u2014", alpha = 1))) +
        scale_color_manual(labels = geom_label,
                           values = category_color,
                           name = category_var)  +
        scale_y_reverse(expand = c(0.2, 0)) +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        theme_bw() + ylab("Distance")

    circular_dendrogram <- dendrogram + coord_polar(theta = "x")

    return(list(dendrogram = dendrogram,
                circular_dendrogram = circular_dendrogram))
}
