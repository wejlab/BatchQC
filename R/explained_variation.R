#' This function allows you to plot explained variation
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @import reshape2
#' @import ggplot2
#' @return List of explained variation by batch and condition
#' @export
EV_plotter <- function(se, batch, condition, assay_name) {
    batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
    EV_boxplot <- ggplot(data =
            melt(as.data.frame(batchqc_ev$explained_variation),
                id.vars = NULL),
        aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "Percent Explained Variation") +
        labs(title = "Percent of Variation Explained by Source") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(EV_boxplot=EV_boxplot))
}
#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#' @export
covariate_pval_plotter <- function(DE_res) {
    pval_table <- c()
    covar_list <- c()
    for (i in seq_len(length(resultsNames(DE_res$dds)))) {
        if (resultsNames(DE_res$dds)[i] == 'Intercept') {
            next
        }
        else if (i == length(resultsNames(DE_res$dds))) {
            next
        }
        else {
            pval_table <- cbind(pval_table,
                results(DE_res$dds,
                    name = resultsNames(DE_res$dds)[i])$pvalue)
            covar_list <- c(covar_list,resultsNames(DE_res$dds)[i])
        }
    }
    colnames(pval_table) <- covar_list
    covar_boxplot <- ggplot(subset(melt(data.table::as.data.table(pval_table),
        id.vars = NULL),
        variable %in% covar_list),
        aes(x = variable, y = value, fill = variable)) +
        geom_violin(width = 0.8) +
        geom_boxplot(width = 0.1) +
        coord_flip() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "P-Values") +
        labs(title="Distribution of Covariate Effects (P-Values) Across Genes")+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(covar_boxplot = covar_boxplot))
}


#' This function allows you to plot batch p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#' @export
batch_pval_plotter <- function(DE_res) {
    batch_boxplot <- ggplot(
        data = melt(data.table::as.data.table(results(DE_res$dds)$pvalue),
            id.vars = NULL),
        aes(x = variable, y = value, fill = variable)) +
        geom_violin(width = 0.8) +
        geom_boxplot(width = 0.1) +
        scale_color_manual(values = "#56B4E9", aesthetics = "fill") +
        coord_flip() +
        scale_x_discrete(name = "", labels = "Batch") +
        scale_y_continuous(name = "P-Values")+
        labs(title="Distribution of Batch Effect (P-Values) Across Genes") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(batch_boxplot = batch_boxplot))
}
