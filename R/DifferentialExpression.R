globalVariables(c("chosen"))

#' Volcano plot
#'
#' This function allows you to plot DE analysis results as a volcano plot
#' @param volcano_data A dataframe of expression change and p-value data
#' @param pslider Magnitude of significance value threshold
#' @param fcslider Magnitude of expression change value threshold
#' @return A volcano plot of expression change and significane value data
#' @import ggplot2
#' @import scran
#' @example R/examples/volcano_plot.R
#'
#' @export
volcano_plot <- function(volcano_data,pslider,fcslider) {

    volcano_data <- as.data.frame(volcano_data)
    
    pslider_factor <- pslider
    pslider_cond <- case_when(volcano_data[, 2] <
                                pslider_factor ~ "TRUE",
                                    volcano_data[, 2] >=
                                pslider_factor ~ "FALSE",
                                    TRUE ~ 'NA')
    fcslider_factor <- fcslider
    fcslider_cond <- case_when(abs(volcano_data[, 1]) <
                                    fcslider_factor ~ "FALSE",
                                abs(volcano_data[, 1]) >=
                                    fcslider_factor ~ "TRUE",
                                    TRUE ~ 'NA')
    filters <- cbind(pslider_cond,fcslider_cond)
    cond <- apply(filters, 1, function(x)(TRUE %in% x))
    slider_cond <- NULL
    volcano_data <- volcano_data %>%
        mutate(slider_cond = cond)

    p <- ggplot(data = volcano_data, aes(x = volcano_data[,1],
                                        y = -log10(volcano_data[,2]),
                                        color = slider_cond)) +
        geom_point() +
        scale_color_manual(values = c('FALSE' = 'red', 'TRUE' = 'blue',
                                        'NA'='black')) +
        xlab("Change in Expression (log2 fold change)") +
        ylab("Signifigance Value (-log10 p-value)") +
        theme(legend.position = "bottom")

    return(p)
}

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
