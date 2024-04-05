#' Heatmap Plotter
#'
#' This function allows you to plot a heatmap
#' @param se SummarizedExperiment
#' @param assay normalized or corrected assay
#' @param nfeature number of features to display
#' @param annotation_column choose column
#' @param log_option TRUE if data should be logged before plotting (recommended
#' for sequencing counts), FALSE if data should not be logged (for instance,
#' data is already logged)
#' @import pheatmap
#' @return heatmap plot
#' @examples
#' library(scran)
#' se <- mockSCE()
#' heatmaps <- BatchQC::heatmap_plotter(se,
#'                                 assay = "counts",
#'                                 nfeature = 15,
#'                                 annotation_column = c("Mutation_Status",
#'                                 "Treatment"), log_option = FALSE)
#' correlation_heatmap <- heatmaps$correlation_heatmap
#' correlation_heatmap
#'
#' heatmap <- heatmaps$topn_heatmap
#' heatmap
#'
#' @export
heatmap_plotter <- function(se, assay, nfeature, annotation_column,
    log_option) {
    data <- preprocess(se, assay, nfeature, log_option = FALSE)

    for (i in seq_len(nrow(data))) {
        data[i, ] <- (data[i, ] - mean(data[i, ])) / stats::sd(data[i, ])
    }

    col_info <- data.frame(colData(se))

    cor <- cor(data)
    if (!is.null(annotation_column)) {
        if (length(annotation_column) == 1) {
            col_info <- data.frame(col_info[, annotation_column],
                                row.names = rownames(col_info))
        }else {
            col_info <- col_info[, annotation_column]
        }

        # Numeric to Character conversion
        col_info <- heatmap_num_to_char_converter(ann_col = col_info)

        correlation_heatmap <- pheatmap(cor,
                                        annotation_col = col_info,
                                        # annotation_colors = ann_colors,
                                        annotation_row = col_info,
                                        show_colnames = FALSE,
                                        show_rownames = FALSE,
                                        annotation_names_col = FALSE,
                                        annotation_names_row = FALSE,
                                        silent = TRUE)

        topn_heatmap <- pheatmap(data, annotation_col = col_info,
                                    # annotation_colors = ann_colors,
                                    show_colnames = FALSE,
                                    annotation_names_col = FALSE,
                                    show_rownames = FALSE,
                                    silent = TRUE)

        return(list(correlation_heatmap = correlation_heatmap,
                    topn_heatmap = topn_heatmap))
    }
}
