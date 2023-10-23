globalVariables(c("chosen"))

#' Differential Expression Analysis
#'
#' This function runs DE analysis on a count matrix in the se object
#' @param se SummarizedExperiment object
#' @param method DE analysis method option
#' @param batch Batch sample metadata column
#' @param conditions Sample metadata columns for additional analysis covariates
#' @param assay_to_analyze Assay for DE analysis
#' @return A named list of of two matrices.
#' @return res features the DE analysis results.
#' @return volcano features a subset of the DE analysis results for plotting.
#' @import SummarizedExperiment
#' @import DESeq2
#' @import scran
#'
#' @export
DE_analyze <- function(se, method, batch, conditions, assay_to_analyze) {
    data <- assays(se)[[assay_to_analyze]]
    rownames(data) <- names(se)
    analysis_design <- as.data.frame(colData(se)[c(conditions, batch)])

    if (method == 'DESeq2') {
        # Check if the assay contains counts (e.g. non negative integer data), otherwise throw an error
        colnames(data) <- rownames(analysis_design)
        data[is.na(data)] <- 0
            dds <- DESeqDataSetFromMatrix(countData = data,
                                        colData = analysis_design,
                                            design = stats::as.formula(
                                                paste(" ~ ",
                                        paste(colnames(analysis_design),
                                            collapse = "+"))))
        dds <- DESeq(dds)  ## Make sure this "dds" is a SummarizedExperiment object (might be a DESeq object)
    }
    if (method == 'limma') { 
        # Solomon is going to add this!!
        # output SummarizedExperiment -> same format as DESEQ above
    }
    return(list(dds = dds))
}


#' Returns summary table for p-values of explained variation
#'
#' @param DE_res DE analysis results output from DE_analyze()
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
pval_summary <- function(DE_res) {

    pval_table <- rbind(summary(results(DE_res$dds)[, 'pvalue']))

    row_count <- 1
    for (i in seq_len(length(resultsNames(DE_res$dds)))) {
        if (resultsNames(DE_res$dds)[i] == 'Intercept') {
            next
        }else if (i == length(resultsNames(DE_res$dds))) {
            next
        }else {
            pval_table <- rbind(pval_table, summary(
                results(DE_res$dds, name = resultsNames(DE_res$dds)[i])$pvalue))
            rownames(pval_table)[row_count + 1] <- resultsNames(DE_res$dds)[i]
            row_count <- row_count + 1
        }
    }

    rownames(pval_table)[1] <- "Batch"

    return(list(pval_table = pval_table))
}


#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
covariate_pval_plotter <- function(DE_res) {
    pval_table <- c()
    covar_list <- c()
    for (i in seq_len(length(resultsNames(DE_res$dds)))) {
        if (resultsNames(DE_res$dds)[i] == 'Intercept') {
            next
        }else if (i == length(resultsNames(DE_res$dds))) {
            next
        }else {
            pval_table <- cbind(pval_table,
                results(DE_res$dds,
                    name = resultsNames(DE_res$dds)[i])$pvalue)
            covar_list <- c(covar_list, resultsNames(DE_res$dds)[i])
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
        labs(title =
                "Distribution of Covariate Effects (P-Values) Across Genes") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(covar_boxplot = covar_boxplot))
}


#' This function allows you to plot batch p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
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
        scale_y_continuous(name = "P-Values") +
        labs(title = "Distribution of Batch Effect (P-Values) Across Genes") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(batch_boxplot = batch_boxplot))
}
