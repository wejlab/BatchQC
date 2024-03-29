#' This function allows you to plot PCA
#' @param se SummarizedExperiment object
#' @param nfeature number of features
#' @param color choose a color
#' @param shape choose a shape
#' @param assays array of assay names from `se`
#' @param xaxisPC the PC to plot as the x axis
#' @param yaxisPC the PC to plot as the y axis
#' @param log_option TRUE if data should be logged before plotting (recommended
#' for sequencing counts), FALSE if data should not be logged (for instance,
#' data is already logged); FALSE by default
#' @usage PCA_plotter(se, nfeature, color, shape, assays, xaxisPC, yaxisPC,
#' log_option = FALSE)
#' @return List containing PCA info, PCA variance and PCA plot
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se_object_ComBat_Seq <- BatchQC::batch_correct(se, method = "ComBat-Seq",
#'                                             assay_to_normalize = "counts",
#'                                             batch = "Mutation_Status",
#'                                             covar = "Treatment",
#'                                             output_assay_name =
#'                                                 "ComBat_Seq_Corrected")
#' pca_plot <- BatchQC::PCA_plotter(se = se_object_ComBat_Seq,
#'                             nfeature = 2, color = "Mutation_Status",
#'                             shape = "Treatment",
#'                             assays = c("counts", "ComBat_Seq_Corrected"),
#'                             xaxisPC = 1, yaxisPC = 2, log_option = FALSE)
#' pca_plot$plot
#' pca_plot$var_explained
#'
#' @export
PCA_plotter <- function(se, nfeature, color, shape,
    assays, xaxisPC, yaxisPC, log_option = FALSE) {
    pca_plot_data <- data.frame()
    var_explained_data <- NULL
    coldata <- data.frame(colData(se))
    for (assay in assays){
        if (! assay %in%  names(se@assays)) {
            warning(sprintf('"%s" is not an available assay', assay))
            next
        }else {
            # Preprocess data
            data <- preprocess(se, assay, nfeature, log_option)
            centered <- data - rowMeans(data) / matrixStats::rowSds(data)
            coldata <- data.frame(colData(se))
            pca <- stats::prcomp(t(centered), center = FALSE)

            # Get variance explained
            var_explained <- summary(pca)$importance["Proportion of Variance", ]
            var_explained_df <- stats::setNames(as.data.frame(var_explained),
                assay)
            if (is.null(var_explained_data)) {
                var_explained_data <- var_explained_df
            }else {
                var_explained_data <- cbind(var_explained_data,
                    var_explained_df)
            }
            # Extract PC data
            pca_data <- as.data.frame(pca$x)

            # Annotate with assay name
            pca_data['assay'] <- assay

            # Merge metadata
            pca_md <- cbind(coldata, pca_data)
            pca_md$sample <- rownames(coldata)

            # Add to data
            pca_plot_data <- rbind(pca_plot_data, pca_md)
        }
    }
    # Reorder data
    pca_plot_data$assay <- factor(pca_plot_data$assay, levels = assays)
    var_explained_data <- var_explained_data[c(xaxisPC, yaxisPC), ,
                                            drop = FALSE]
    plot <- plot_data(pca_plot_data, color, shape, xaxisPC, yaxisPC)

    return(list(PCA = pca_plot_data, var_explained = var_explained_data,
        plot = plot))
}

#' This function formats the PCA plot using ggplot
#' @param pca_plot_data Data for all assays to plot
#' @param color variable that will be plotted as color
#' @param shape variable that will be plotted as shape
#' @param xaxisPC the PC to plot as the x axis
#' @param yaxisPC the PC to plot as the y axis
#' @import ggplot2
#' @return PCA plot

plot_data <- function(pca_plot_data, color, shape, xaxisPC, yaxisPC) {
    pca_plot_data[, c(color)] <- factor(pca_plot_data[, color])
    pca_plot_data[, c(shape)] <- factor(pca_plot_data[, shape])

    xaxisPC <- paste0('PC', xaxisPC)
    yaxisPC <- paste0('PC', yaxisPC)
    PCAplot <- ggplot(pca_plot_data,
        aes_string(x = xaxisPC,
            y = yaxisPC,
            color = color,
            shape = shape,
            sample = 'sample')) +
        geom_point(size = 3) +
        facet_wrap(vars(assay), ncol = 2, scales = 'free')

    return(PCAplot)
}
