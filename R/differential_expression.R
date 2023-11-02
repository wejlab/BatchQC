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
    res <- list()

    if (method == 'DESeq2') {
        # Check if the assay contains counts (e.g. non negative integer data),
        # otherwise throw an error
        colnames(data) <- rownames(analysis_design)
        data[is.na(data)] <- 0
            dds <- DESeqDataSetFromMatrix(countData = data,
                                        colData = analysis_design,
                                            design = stats::as.formula(
                                                paste(" ~ ",
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
        # Solomon is going to add this!!
        # output SummarizedExperiment -> same format as DESEQ above
        # output SummarizedExperiment -> same format as DESEQ above
        # Define the design matrix
        design <- model.matrix(stats::as.formula(
            paste(
                " ~ ",
                paste(colnames(analysis_design),
                    collapse = "+"
                )
            )
        ), data = analysis_design)
        # Fit the linear model
        fit <- limma::lmFit(data, design)
        # #Introducing Contracts
        # contrasts <- makeContrasts(contrasts=NULL, levels = design)
        # fit2 <- contrasts.fit(fit, contrasts)
        # Apply empirical Bayes moderation to the standard errors
        fit2 <- eBayes(fit)
        # Extract top differentially expressed genes
        res <- topTable(fit2, number = Inf)
    } else if (method == 't') { #need to ensure proper output
        res <- findMarkers(data,
            analysis_design[,1],
            test.type = method,
            pval.type = "all",
            lfc = 1)
        res <- as.matrix(res[[1]])
        pvalue <- res[,2]
        log2FC <- res[,4]
        res <- res[order(res[,1], decreasing = FALSE), ]
        to_plot <- cbind(log2FC, pvalue) #volcano info
    } else if (method == 'wilcox') {#need to ensure proper output #See Aug 2nd github for reference
        res <- findMarkers(data,
            analysis_design[,1],
            test.type = method,
            pval.type = "all",
            lfc = 1)
        res <- as.matrix(res[[1]])
        pvalue <- res[,2]
        AUC <- res[,3]
        res <- res[order(res[,1], decreasing=FALSE), ]
        to_plot <- cbind(AUC, pvalue) #volcano info
    }
    return(res) #return... the se object where the rowData contains baseMean, log2FOldChange, lfcSe, stat, pvalue, and padj
}


#' Returns summary table for p-values of explained variation
#'
#' @param pvals Differential Expression analysis result (a named list of
#' dataframes corresponding to each analysis completed with a "pvalue" column)
#' @return summary table for p-values of explained variation for each analysis
#'
#' @export
pval_summary <- function(res_list) {

    pval_sum_table <- data.frame()
    for(res_table in res_list){
        pval_sum_table <- rbind(summary(pval_sum_table, res_table$pvalue))
    }

    row.names(pval_sum_table) <- names(res_list)

    return(pval_table = pval_sum_table)

    # pval_table <- rbind(summary(results(dds)[, 'pvalue']))
    #
    # row_count <- 1
    # for (i in seq_len(length(resultsNames(dds)))) {
    #     if (resultsNames(dds)[i] == 'Intercept') {
    #         next
    #     }else if (i == length(resultsNames(dds))) {
    #         next
    #     }else {
    #         pval_table <- rbind(pval_table, summary(
    #             results(dds, name = resultsNames(dds)[i])$pvalue))
    #         rownames(pval_table)[row_count + 1] <- resultsNames(dds)[i]
    #         row_count <- row_count + 1
    #     }
    # }
    #
    # rownames(pval_table)[1] <- "Batch"
    #
    # return(list(pval_table = pval_table))
}


#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param DE_Results Differential Expression analysis result (a named list of
#' dataframes corresponding to each analysis completed with a "pvalue" column)
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return boxplots of pvalues for each condition
#'
#' @export
covariate_pval_plotter <- function(DE_results) {
    pval_table <- data.frame(row.names = row.names(DE_results[[1]])) #need to create this to be the size of the genes orinally here
    for(covar in DE_results){
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
                "Distribution of Covariate Effects (P-Values) Across Genes") +
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
