globalVariables(c("chosen"))

#' Volcano plot
#' 
#' This function allows you to plot DE analysis results as a volcano plot
#' @param volcano_data A dataframe of expression change and p-value data
#' @param slider Magnitude of significance value threshold
#' @return A volcano plot of expression change and significane value data
#' @import ggplot2
#' @example R/examples/volcano_plot.R
#' 
#' @export
volcano_plot <- function(volcano_data,slider) {
    
    slider_cond <- NULL
    volcano_data <- as.data.frame(volcano_data)
    slider_factor <- (1 * (10^slider))
    volcano_data <- volcano_data %>%
        mutate(slider_cond = case_when(volcano_data[, 2] < 
                                            slider_factor ~ "TRUE", 
                                        volcano_data[, 2] >= 
                                            slider_factor ~ "FALSE",
                                        TRUE ~ 'NA'))
    
    p <- ggplot(data = volcano_data, aes(x = volcano_data[,1],
                                        y = -log10(volcano_data[,2]),
                                        color = slider_cond)) + 
        geom_point() +
        scale_color_manual(values = c('FALSE' = 'red', 'TRUE' = 'orange',
                                        'NA'='black')) + 
        xlab("Change in Expression") +
        ylab("Signifigance Value") +
        theme(legend.position="bottom")
    
    return(p)
}

#' Differential Expression Analysis
#' 
#' This function runs DE analysis on a count matrix in the se object
#' @param se SummarizedExperiment object
#' @param method DE analysis method options t-test, Wilcox rank sum, and DESeq2
#' @param conditions Sample metadata column for DE analysis
#' @param assay_to_analyze Assay for DE analysis
#' @return A named list of of two matrices. 
#' @return res features the DE analysis results.
#' @return volcano features a subset of the DE analysis results for plotting.
#' @import SummarizedExperiment
#' @import scran
#' @import DESeq2
#' @example R/examples/DE_analyze.R
#'
#' @export
DE_analyze <- function(se, method, conditions, assay_to_analyze) {
    data <- se@assays@data[[assay_to_analyze]]
    rownames(data) <- names(se)
    analysis_design <- as.data.frame(se@colData[conditions])
    con <- paste("~ ",colnames(analysis_design))
    
    if (method == 't') {
        res <- findMarkers(data, 
                           analysis_design[,1],
                           test.type  = method,
                           pval.type = "all", 
                           lfc = 1)
        res <- as.matrix(res[[1]])
        pvalue <- res[,2]
        log2FC <- res[,4]
        res <- res[order(res[,1], decreasing = FALSE), ]
        to_plot <- cbind(log2FC, pvalue)
    }
    else if (method == 'wilcox') {
        res <- findMarkers(data, 
                           analysis_design[,1],
                           test.type = method, 
                           pval.type = "all", 
                           lfc=1)
        res <- as.matrix(res[[1]])
        pvalue <- res[,2]
        AUC <- res[,3]
        res <- res[order(res[,1], decreasing=FALSE), ]
        to_plot <- cbind(AUC, pvalue)
    }
    else if (method == 'DESeq2') {
        colnames(data) <- rownames(analysis_design)
        data[is.na(data)] <- 0
        dds <- DESeqDataSetFromMatrix(countData = abs(round(data)),
                                        colData = analysis_design,
                                        design = stats::as.formula(con))
        res <- DESeq(dds)
        res <- results(res)
        pvalue <- res$pvalue
        log2FC <- res$log2FoldChange
        res <- as.matrix(res)
        res <- res[order(res[,6], decreasing = FALSE), ]
        to_plot <- cbind(log2FC, pvalue)
    }
    return(list(res = res,
                volcano = to_plot))
}
