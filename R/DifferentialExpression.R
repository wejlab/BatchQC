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
