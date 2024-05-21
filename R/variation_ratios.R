#' Creates Ratios of batch to variable variation statistic
#'
#' @param ex_variation_table table of explained variation results from
#'   batchqc_explained_variation
#' @param batch batch
#' @return dataframe with condition/batch ratios
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' se$Treatment <- as.factor(se$Treatment)
#' expl_var_result <- batchqc_explained_variation(se, batch = "Mutation_Status",
#'                             condition = "Treatment", assay_name = "counts")
#' ratios_results <- variation_ratios(expl_var_result[[1]],
#'     batch = "Mutation_Status")
#' ratios_results
#' @export

variation_ratios <- function(ex_variation_table, batch) {
    ratio_variation_table <- as.data.frame(ex_variation_table[[batch]])
    colnames(ratio_variation_table) <- c(batch)
    for (column in names(ex_variation_table)) {
        if (column != batch & column != "Explained" & column != "Unexplained") {
            col_div_batch <- ex_variation_table[[column]] /
                ex_variation_table[[batch]]
            new_col <- paste0(column, "/", batch)
            ratio_variation_table[[new_col]] <- col_div_batch
        }
    }
    ratio_variation_table <- select(ratio_variation_table, -batch)
    row.names(ratio_variation_table) <- row.names(ex_variation_table)
    return(ratio_variation_table)
}

#' This function allows you to plot ratios of explained variation
#' @param ev_ratio table of ratios from variation_ratios()
#' @import reshape2
#' @import ggplot2
#' @return boxplot of ratios
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' se$Treatment <- as.factor(se$Treatment)
#' expl_var_result <- batchqc_explained_variation(se, batch = "Mutation_Status",
#'                             condition = "Treatment", assay_name = "counts")
#' ratios_results <- variation_ratios(expl_var_result[[1]],
#'     batch = "Mutation_Status")
#' ratio_boxplot <- BatchQC::ratio_plotter(ratios_results)
#' ratio_boxplot
#'
#' @export
ratio_plotter <- function(ev_ratio) {
    ratio_boxplot <- ggplot(data =
            melt(as.data.frame(ev_ratio),
                id.vars = NULL),
        aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "Variation Source") +
        labs(title = "Ratio of Variation") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(ratio_boxplot)
}
