
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

    coldata <- data.frame(colData(se))

    cor <- cor(data)
    if (!is.null(annotation_column)) {
        if (length(annotation_column) == 1) {
            coldata <- data.frame(coldata[, annotation_column],
                row.names = rownames(coldata))
        }else {
            coldata <- coldata[, annotation_column]
        }
        correlation_heatmap <- pheatmap(cor, annotation_col = coldata,
            annotation_row = coldata,
            show_colnames = FALSE,
            show_rownames = FALSE,
            annotation_names_col = FALSE,
            annotation_names_row = FALSE,
            silent = TRUE)

        topn_heatmap <- pheatmap(data, annotation_col = coldata,
            show_colnames = FALSE,
            annotation_names_col = FALSE,
            show_rownames = FALSE,
            silent = TRUE)
    }else {
        correlation_heatmap <- pheatmap(cor, show_colnames = FALSE,
            show_rownames = FALSE,
            annotation_names_col = FALSE,
            annotation_names_row = FALSE,
            silent = TRUE)

        topn_heatmap <- pheatmap(data, show_colnames = FALSE,
            annotation_names_col = FALSE,
            show_rownames = FALSE,
            silent = TRUE)
    }

    return(list(correlation_heatmap = correlation_heatmap,
        topn_heatmap = topn_heatmap))
}
