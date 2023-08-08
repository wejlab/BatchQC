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
#' @example R/examples/DE_analyze.R
#'
#' @export
DE_analyze <- function(se, method, batch, conditions, assay_to_analyze) {
    data <- se@assays@data[[assay_to_analyze]]
    rownames(data) <- names(se)
    analysis_design <- as.data.frame(se@colData[c(conditions,batch)])

    if (method == 'DESeq2') {
        colnames(data) <- rownames(analysis_design)
        data[is.na(data)] <- 0
            dds <- DESeqDataSetFromMatrix(countData = abs(round(data)),
                                        colData = analysis_design,
                                            design = stats::as.formula(
                                                paste(" ~ ",
                                        paste(colnames(analysis_design),
                                            collapse= "+"))))
        dds <- DESeq(dds)
    }
    return(list(dds = dds))
}


#' Returns summary table for p-values of explained variation
#'
#' @param DE_res DE analysis results output from DE_analyze()
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#' @export
pval_summary <- function(DE_res) {

    pval_table <- rbind(summary(results(DE_res$dds)[,'pvalue']))

    row_count <- 1
    for (i in seq_len(length(resultsNames(DE_res$dds)))) {
        if (resultsNames(DE_res$dds)[i] == 'Intercept') {
            next
        }
        else if (i == length(resultsNames(DE_res$dds))) {
            next
        }
        else {
            pval_table <- rbind(pval_table, summary(
                results(DE_res$dds,name = resultsNames(DE_res$dds)[i])$pvalue))
            rownames(pval_table)[row_count + 1] <- resultsNames(DE_res$dds)[i]
            row_count <- row_count + 1
        }
    }

    rownames(pval_table)[1] <- "Batch"

    return(list(pval_table=pval_table))
}
