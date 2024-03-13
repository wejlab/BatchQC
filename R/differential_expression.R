globalVariables(c("chosen", "P.Value", "adj.P.Val"))

#' Differential Expression Analysis
#'
#' This function runs DE analysis on a count matrix (DESeq) or a normalized log
#' or log-CPM matrix (limma, t-test, anova, wilcox) in the se object
#' @param se SummarizedExperiment object
#' @param method DE analysis method option
#' @param batch Batch sample metadata column
#' @param conditions Sample metadata columns for additional analysis covariates
#' @param assay_to_analyze Assay for DE analysis
#' @param include_batch boolean include batch in the analysis (default = TRUE)
#' @param condition_of_interest sample metadata to perform limma with
#' @return A named list of of two matrices.
#' @return res features the DE analysis results.
#' @return volcano features a subset of the DE analysis results for plotting.
#' @import SummarizedExperiment
#' @import DESeq2
#' @import scran
#' @importFrom stats model.matrix as.formula t.test aov coef
#' @importFrom limma lmFit eBayes topTable makeContrasts contrasts.fit
#'
#' @export
DE_analyze <- function(se, method, batch, conditions, assay_to_analyze,
    include_batch = TRUE, condition_of_interest = NULL) {
    data <- assays(se)[[assay_to_analyze]]
    rownames(data) <- names(se)

    if (include_batch & (!is.null(condition_of_interest) & condition_of_interest != "")) {
        analysis_design <- as.data.frame(colData(se)[c(condition_of_interest,
            conditions, batch)])
    }else if (include_batch & (is.null(condition_of_interest) | condition_of_interest == "")) {
        analysis_design <- as.data.frame(colData(se)[c(conditions, batch)])
    }else if (!include_batch & (!is.null(condition_of_interest) & condition_of_interest != "")) {
        analysis_design <- as.data.frame(colData(se)[c(condition_of_interest,
            conditions)])
    }else {
        analysis_design <- as.data.frame(colData(se)[c(conditions)])
    }

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
        if (condition_of_interest == "") {
           print("Error: must provide condition of interest")
            #need to throw error in shiny
            return()
        }
    #     analysis_design <- tidyr::unite(analysis_design,
    #         col = "group",
    #         sep = "_",
    #         remove = FALSE)

        design <- stats::model.matrix(stats::as.formula(paste(" ~0 + ",
               paste(colnames(analysis_design), collapse = "+"))),
            data = analysis_design)
        # Fit the linear model

         fit <- limma::lmFit(data, design)

         #create contrasts
         cond_factors <- unique(colData(se)[[condition_of_interest]])
         contr_string <- list()
         for (i in 1:(length(cond_factors) - 1)) {
             k <- i + 1
             for (j in k:length(cond_factors)){
                 contr_string <- c(contr_string, paste(colnames(stats::coef(fit))[[i]], "-", colnames(stats::coef(fit))[[j]]))
             }
         }
         myargs <- list(contrasts = contr_string, levels = design)
         contr <- do.call(makeContrasts, myargs)
         #contr <- limma::makeContrasts(colnames(coef(fit))[[i]] - colnames(coef(fit))[[j]], levels = colnames(design))
                 cont_est <- limma::contrasts.fit(fit, contr)
                 cont_est <- limma::eBayes(cont_est)

                 total_contrasts <- match("AveExpr", names(topTable(cont_est))) - 1
                 for (i in 1:total_contrasts){
                     results <- limma::topTable(cont_est, number = Inf) %>%
                         select(c(i, P.Value, adj.P.Val))
                     colnames(results) <- c("log2FoldChange", "pvalue", "padj" )
                     res[[contr_string[[i]]]] <- results #show Evan here and ask if this is a good way to display the results, logfoldchange is what differs
                 }
                 # results <- limma::topTable(cont_est, number = Inf) %>%
                 #     select(c(logFC, P.Value, adj.P.Val))
                 # colnames(results) <- c("log2FoldChange", "pvalue", "padj" )
                 # res[[contr_string[[1]]]] <- results
                 #contrastName <- paste0(item, "-", colnames(coef(fit))[[j]])
                 #res[[contrastName]] <- results

        # contr <- limma::makeContrasts(i - j, levels = colnames(design))
        # cont_est <- limma::contrasts.fit(fit, contr)
        # cont_est <- limma::eBayes(cont_est)
        # results <- limma::topTable(cont_est, number = Inf) %>%
        #     select(c(logFC, P.Value, adj.P.Val))
        # colnames(results) <- c("log2FoldChange", "pvalue", "padj" )
        # contrastName <- paste0(item, "-", colnames(coef(fit))[[j]])
        # res[[contrastName]] <- results

        # for (i in 1:(length(colnames(coef(fit)))-1)){
        #     item <- colnames(coef(fit))[[i]]
        #     seconditem <- i + 1
        #     if (item != "(Intercept)") {
        #         for(j in seconditem:length(colnames(coef(fit)))){
        #             contr <- limma::makeContrasts(i - j, levels = colnames(coef(fit)))
        #             cont_est <- limma::contrasts.fit(fit, contr)
        #             cont_est <- limma::eBayes(cont_est)
        #             results <- limma::topTable(cont_est, number = Inf) %>%
        #                 select(c(logFC, P.Value, adj.P.Val))
        #             colnames(results) <- c("log2FoldChange", "pvalue", "padj" )
        #             contrastName <- paste0(item, "-", colnames(coef(fit))[[j]])
        #             res[[contrastName]] <- results
        #         }
        #     }
        # }
        # #Introducing Contracts
        # contrasts <- makeContrasts(contrasts=NULL, levels = design)
        # fit2 <- contrasts.fit(fit, contrasts)
        # Apply empirical Bayes moderation to the standard errors
        #fit2 <- limma::eBayes(fit)
        # Extract top differentially expressed genes
        #lim_res <- limma::topTable(fit2, number = Inf)
        #res <-

    } else if (method == 't-test') { #need to ensure proper output
        # loop through batch and all conditions
        for (variable in colnames(analysis_design)) {
            if (length(unique(analysis_design$variable)) == 2) {
                t_results <- stats::t.test(analysis_design$variable)
            }else if (length(unique(analysis_design$variable)) > 2) {
                anova_results <- stats::aov()
            }
        }

        # retrieve needed results form each and create a dataframe for each variable
        # create a list of dataframes and return

        # res_ttest <- findMarkers(data,
        #     analysis_design[,1],
        #     test.type = "t",
        #     pval.type = "all",
        #     lfc = 1)
        # for(t_res in names(res_ttest)){
        #     res_to_add <- data.frame("log2FoldChange" = res_ttest[[t_res]][4],
        #         "pvalue" = res_ttest[[t_res]][1],
        #         "padj" = res_ttest[[t_res]][2],
        #         row.names = rownames(res_ttest[[t_res]]))
        #     #res_to_add <- as.data.frame(res_to_add)
        #     #row.names(res_to_add) <- row.names(res_ttest[[t_res]])
        #     colnames(res_to_add) <- c("log2FoldChange", "pvalue", "padj")
        #     res[[t_res]] <- res_to_add
        #}
        # res_ttest <- as.matrix(res_ttest[[1]])
        # pvalue <- res_ttest[,2]
        # log2FC <- res_ttest[,4]
        # res_ttest <- res_ttest[order(res[,1], decreasing = FALSE), ]
        # to_plot <- cbind(log2FC, pvalue) #volcano info
    }else if (method == 'wilcox') {#need to ensure proper output #See Aug 2nd github for reference
        res <- findMarkers(data,
            analysis_design[, 1],
            test.type = method,
            pval.type = "all",
            lfc = 1)
        res <- as.matrix(res[[1]])
        pvalue <- res[, 2]
        AUC <- res[, 3]
        res <- res[order(res[, 1], decreasing = FALSE), ]
        to_plot <- cbind(AUC, pvalue) #volcano info
    }
    return(res) #return... each analysis with log2FOldChange, pvalue, and padj
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
