globalVariables(c("chosen", "P.Value", "adj.P.Val"))

#' Differential Expression Analysis
#'
#' This function runs DE analysis on a count matrix (DESeq) or a normalized log
#' or log-CPM matrix (limma) contained in the se object
#' @param se SummarizedExperiment object
#' @param method DE analysis method option (either 'DESeq' or 'limma')
#' @param batch metadata column in the se object representing batch
#' @param conditions metadata columns in the se object representing additional analysis covariates
#' @param assay_to_analyze Assay in the se object (either counts for DESeq or normalized data for limma) for DE analysis
#' @return A named list containing the log2FoldChange, pvalue and adjusted pvalue (padj) for each analysis returned by DESeq or limma
#' @import SummarizedExperiment
#' @import DESeq2
#' @import scran
#' @importFrom stats model.matrix as.formula t.test aov coef
#' @importFrom limma lmFit eBayes topTable makeContrasts contrasts.fit
#'
#' @export
DE_analyze <- function(se, method, batch, conditions, assay_to_analyze) {
    data <- assays(se)[[assay_to_analyze]]
    rownames(data) <- names(se)

    analysis_design <- as.data.frame(colData(se)[c(conditions, batch)])

    res <- list()

    if (method == 'DESeq2') {
        # Check if the assay contains counts (e.g. non negative integer data),
        for (item in data){
            if (!is.integer(item)) {
                print("Error: data contains non-integers")
                #need to throw error in shiny
                return()
            }else if (item < 0) {
                print("Error: data contains negative integers")
                #need to throw error in shiny
                return()
            }
        }
        # otherwise throw an error
        colnames(data) <- rownames(analysis_design)
        data[is.na(data)] <- 0
            dds <- DESeqDataSetFromMatrix(countData = data,
                                        colData = analysis_design,
                                        design = stats::as.formula(paste(" ~ ",
                                            paste(colnames(analysis_design),
                                            collapse = "+"))))
        dds <- DESeq(dds)

        for (covar in DESeq2::resultsNames(dds)){
            imp_data <- data.frame("log2FoldChange" =
                    DESeq2::results(dds, name = covar)$log2FoldChange,
                "pvalue" =  DESeq2::results(dds, name = covar)$pvalue,
                "padj" = DESeq2::results(dds, name = covar)$padj,
                row.names = rownames(DESeq2::results(dds, name = covar)))
           res[[covar]] <- imp_data
        }
    }else if (method == 'limma') {

        design <- stats::model.matrix(stats::as.formula(paste(" ~",
               paste(colnames(analysis_design), collapse = "+"))),
            data = analysis_design)

         fit <- limma::lmFit(data, design)
         eBayes_res <- limma::eBayes(fit)

         for (i in 1:length(colnames(eBayes_res$coefficients))){
             results <- limma::topTable(eBayes_res, coef = i, number = Inf) %>%
                 select(c(1, P.Value, adj.P.Val))
             colnames(results) <- c("log2FoldChange", "pvalue", "padj" )
             res[[colnames(eBayes_res$coefficients)[[i]]]] <- results
        }

    } else {
        "Error: Please select a method 'DESeq2' or 'limma'"
    }
    return(res)
}


#' Returns summary table for p-values of explained variation
#'
#' @param res_list Differential Expression analysis result (a named list of
#' dataframes corresponding to each analysis completed with a "pvalue" column)
#' @return summary table for p-values of explained variation for each analysis
#'
#' @export
pval_summary <- function(res_list) {

    pval_sum_table <- vector()
    for (res_table in res_list){
        pval_sum_table <- as.data.frame(cbind(pval_sum_table, res_table$pvalue))

    }

    colnames(pval_sum_table) <- names(res_list)
    rownames(pval_sum_table) <- rownames(res_list[[1]])

    return(pval_table = pval_sum_table)
}


#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param DE_results Differential Expression analysis result (a named list of
#' dataframes corresponding to each analysis completed with a "pvalue" column)
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return boxplots of pvalues for each condition
#'
#' @export
covariate_pval_plotter <- function(DE_results) {
    pval_table <- data.frame(row.names = row.names(DE_results[[1]])) #need to create this to be the size of the genes orinally here
    for (covar in DE_results){
        pval_table <- cbind(pval_table, covar$pvalue)
    }

    colnames(pval_table) <- names(DE_results)

    covar_boxplot <- ggplot(subset(melt(data.table::as.data.table(pval_table),
        id.vars = NULL),
        variable %in% names(DE_results)),
        aes(x = variable, y = value, fill = variable)) +
        geom_violin(width = 0.8) +
        geom_boxplot(width = 0.1) +
        coord_flip() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "P-Values") +
        labs(title =
                "Distribution of Batch and Covariate Effects (P-Values) Across Genes") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(covar_boxplot = covar_boxplot)
}


#' This function allows you to plot batch p-values of explained variation
#' @param dds DESeq2 analysis results output
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
batch_pval_plotter <- function(dds) {
    batch_boxplot <- ggplot(
        data = melt(data.table::as.data.table(results(dds)$pvalue),
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
