#' This function calculates p-values for each gene given counts, estimated
#' NB size, and estimated NB mean
#' @param counts a vector of gene expression values (in counts)
#' @param size an estimated size parameter of the NB distributions for the gene
#' @param mu a vector of estimated mu parameter of the NB distributions for
#'   different samples of the gene
#' @importFrom stats pnbinom ks.test
#' @return a p-value based on estimated NB size and mean
#' @keywords internal

counts2pvalue <- function(counts, size, mu) {
    counts <- as.numeric(counts)
    if (max(counts) <= 3) {
        p.fit <- NA
    }else {
        p <- pnbinom(counts, size = size, mu = mu)
        p.fit <- ks.test(p, 'punif')$p.value
    }
    return(p.fit)
}

#' This function calculates goodness-of-fit p-values for each condition
#' level for each gene size, and estimated NB mean
#' @param condition string; name of the se colData with the condition status
#' @param size numeric; an estimated size parameter of the NB distributions for
#'   the gene
#' @param mu_matrix matrix;estimated mu parameter of the NB distributions for
#'   different samples of each gene
#' @param count_matrix string; name of the assay with gene expression matrix
#'   (in counts)
#' @return a data frame of p-values for each gene under each biological
#'   condition
#' @keywords internal

pvalues_all_genes <- function(condition, size, mu_matrix, count_matrix) {
    # Count the number of levels in condition
    unique_conditions <- unique(condition)

    all_pvalues <- vapply(seq_len(length(unique_conditions)), function(j) {
        index_j <- which(condition == unique_conditions[j])
        # For one condition level, calculate the goodness-of-fit p-values
        pvalues_level <- vapply(seq_len(length(size)), function(i) {
            mu_gene <- mu_matrix[i, index_j]
            count_condition <- count_matrix[i, index_j]
            pvalue <- counts2pvalue(counts = count_condition,
                size = size[i],
                mu = mu_gene)
            return(pvalue)
        }, double(1))
        return(pvalues_level)
    }, double(length(size)))

    all_pvalues <- as.data.frame(all_pvalues,
        row.names = row.names(count_matrix))

    colnames(all_pvalues) <- unique_conditions

    return(all_pvalues)
}

#' Performs down sampling for negative binomial model fit check.
#'
#' @param count_matrix matrix; contains the feature data (should be counts)
#' @param num_genes integer; number of genes to use in down sampling
#' @return list containing "sampled" a down sampled assay and "count_matrix"
#'   with the down sampled count_matrix
#' @keywords internal

nb_down_sample <- function(count_matrix, num_genes) {
    # Ensure the number of genes is greater than the desired number for sampling
    if (dim(count_matrix)[1] < num_genes) {
        num_genes <- dim(count_matrix)[1]
    }

    # Down sample
    if (dim(count_matrix)[1] > num_genes) {
        sampled <- sample(row.names(count_matrix), num_genes)
        col_names_prior <- colnames(count_matrix)
        count_matrix <- count_matrix[sampled, ]
    }else {
        sampled <- row.names(count_matrix)
    }

    return(list(sampled = sampled, count_matrix = count_matrix))
}

#' This function calculates goodness-of-fit pvalues for all genes by looking
#' at how the NB model by edgeR or DESeq2 fit the data
#' @import DESeq2
#' @import edgeR
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @param se the se object; se object where all the data is contained
#' @param count_matrix string; name of the assay with gene expression matrix
#'   (in counts)
#' @param condition string; name of the se colData with the condition status
#' @param other_variables string; name of the se colData containing other
#'   variables of interest that should be considered in the model
#' @param method string; method to use for the parametric or non-parametric
#'   test;either "DESeq2" or "edgeR"; default is "edgeR"
#' @param num_genes down sample value, default is 500 (or all genes if less)
#' @param small_sample_cutoff value at which non-parametric test will be used
#'   (considered "large sample size") vs parametric will be used (considered
#'   "small sample size); default is 21
#' @usage goodness_of_fit_nb(
#'     se,
#'     count_matrix,
#'     condition,
#'     other_variables = NULL,
#'     method = "edgeR",
#'     num_genes = 500,
#'     small_sample_cutoff = 21)
#' @return a matrix of p-values where each row is a gene and each column is a
#'   level within the condition of interest
#' @export
#' @examples
#' # example code for small sample
#' library(scran)
#' se <- mockSCE(ncells = 20)
#' se$Treatment <- as.factor(se$Treatment)
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' nb_results <- goodness_of_fit_nb(se = se, count_matrix = "counts",
#'     condition = "Treatment", other_variables = "Mutation_Status",
#'     method = "edgeR")
#' nb_results[1]
#' nb_results[2]
#' nb_results[3]
#'
#' # example code for large sample
#' library(scran)
#' se <- mockSCE(ncells = 150)
#' se$Treatment <- as.factor(se$Treatment)
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' nb_results <- goodness_of_fit_nb(se = se, count_matrix = "counts",
#'     condition = "Treatment", other_variables = "Mutation_Status",
#'     method = "edgeR")
#' nb_results[1]
#' nb_results[2]
#' nb_results[3]

goodness_of_fit_nb <- function(se, count_matrix, condition,
    other_variables = NULL, method = "edgeR", num_genes = 500,
    small_sample_cutoff = 21) {
    # Obtain needed data from se object
    count_matrix <- SummarizedExperiment::assays(se)[[count_matrix]]
    condition <- SummarizedExperiment::colData(se)[[condition]]
    condition <- as.factor(condition)
    num_samples <- dim(count_matrix)[2]
    dw_sample <- nb_down_sample(count_matrix, num_genes)
    count_matrix <- dw_sample$count_matrix
    sampled <- dw_sample$sampled
    conditions_df <- NULL
    model_formula <- ""

    if (!is.null(other_variables)) {
        for (i in seq_len(length(other_variables))) {
            conditions_df <- DataFrame(c(conditions_df,
                SummarizedExperiment::colData(se)[[other_variables[i]]]))
            model_formula <- paste0(model_formula,
                " + ",
                other_variables[i])
        }
    }
    colnames(conditions_df) <- other_variables
    for (i in seq_len(length(colnames(conditions_df)))){
        conditions_df[, i] <- as.factor(conditions_df[, i])
    }

    if (method == "DESeq2") {
        if (num_samples < small_sample_cutoff) {
            result <- DESeq2_small_size(count_matrix, condition,
                other_variables, conditions_df, model_formula, num_samples,
                small_sample_cutoff)
        }else {
            result <- DESeq_large_analysis(count_matrix, condition,
                other_variables, conditions_df, model_formula, num_samples,
                sampled, small_sample_cutoff)
        }
    } else if (method == "edgeR") {
        if (num_samples < small_sample_cutoff) {
            result <- edgeR_small_size(count_matrix, condition, other_variables,
                conditions_df, model_formula, num_samples, small_sample_cutoff)
        }else {
            result <- edgeR_large_analysis(count_matrix, condition,
                other_variables, conditions_df, model_formula, num_samples,
                sampled, small_sample_cutoff)
        }
    }
    return(result)
}

#' This function calculated the goodness of fit of DESeq2 for small sample
#' sizes (intended for less than 20 samples).
#' @import DESeq2
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @param num_samples total number of samples to analyze
#' @param small_sample_cutoff value at which non-parametric test was used
#'   (considered "large sample size") vs parametric was used (considered
#'   "small sample size")
#' @usage DESeq2_small_size(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula,
#'     num_samples,
#'     small_sample_cutoff)
#' @return a list containing the string recommendation, the histogram and a
#'   reference for the original source of the test
DESeq2_small_size <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula, num_samples, small_sample_cutoff) {
    # Use DESeq2 to fit the NB model
    if (is.null(other_variables)) {
        dds <- DESeqDataSetFromMatrix(count_matrix,
            S4Vectors::DataFrame(condition), ~ condition)
    }else {
        dds <- DESeqDataSetFromMatrix(count_matrix,
            S4Vectors::DataFrame(condition, conditions_df),
            as.formula(paste0("~ condition", model_formula)))
    }
    dds <- DESeq(dds)
    # The size parameters estimated by DESeq2 for each gene
    size <- 1 / dispersions(dds)
    # The mu parameters estimated by DESeq2 for each count
    mu_matrix <- assays(dds)[["mu"]]
    # Count the number of levels in condition
    unique_conditions <- unique(condition)
    num_unique_conditions <- length(unique_conditions)

    # For each condition level, get goodness-of-fit p-values for each genes
    all_pvalues <- vapply(seq_len(length(unique_conditions)), function(j) {
        index_j <- which(condition == unique_conditions[j])
        # For one condition level, calculate the goodness-of-fit p-values
        pvalues_level <-  vapply(seq_len(length(size)), function(i) {
            mu_gene <- mu_matrix[i, index_j]
            count_condition <- count_matrix[i, index_j]
            pvalue <- counts2pvalue(counts = count_condition,
                size = size[i],
                mu = mu_gene)
            return(pvalue)
        }, double(1))
        return(pvalues_level)
    }, double(length(size)))

    all_pvalues <- as.data.frame(all_pvalues, row.names =
            row.names(count_matrix))
    colnames(all_pvalues) <- unique_conditions
    recommendation <- nb_proportion(all_pvalues, 0.01, 0.01, num_samples,
        small_sample_cutoff, method = "DESeq2")
    res_histogram <- nb_histogram(all_pvalues)
    reference <- paste0("Adapted for small sample sizes from: Li, Y., ",
        "Ge, X., Peng, F. et al. Exaggerated false positives by popular ",
        "differential expression methods when analyzing human population ",
        "samples. Genome Biol 23, 79 (2022). ",
        "https://doi.org/10.1186/s13059-022-02648-4")
    return(list(recommendation = recommendation, res_histogram = res_histogram,
        reference = reference))
}

#' This function calculated the goodness of fit of DESeq2 for larger sample
#' sizes (intended for more than 150 samples).
#' @import DESeq2
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom stats na.omit
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @param num_samples total number of samples to analyze
#' @param sampled the down sampled matrix
#' @param small_sample_cutoff value at which non-parametric test was used
#'   (considered "large sample size") vs parametric was used (considered
#'   "small sample size")
#' @usage DESeq_large_analysis(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula,
#'     num_samples,
#'     sampled,
#'     small_sample_cutoff)
#' @return a list containing the string recommendation

DESeq_large_analysis <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula, num_samples, sampled,
    small_sample_cutoff) {
    considered_significant <- 0.05
    dds <- permuted_DESeq(count_matrix, condition, other_variables,
        conditions_df, model_formula)
    res <- results(dds)
    # count the number of DEGs
    num_DEGs <- sum(res$padj <= considered_significant)
    all_padj_values <- NULL
    all_pvalues <- NULL
    for (i in 2:length(resultsNames(dds))){
        padj_values <- as.data.frame(results(dds,
            name = resultsNames(dds)[i])$padj, row.names = sampled)
        all_padj_values <- as.data.frame(c(all_padj_values, padj_values))
        p_values <- as.data.frame(results(dds,
            name = resultsNames(dds)[i])$pvalue, row.names = sampled)
        all_pvalues <- as.data.frame(c(all_pvalues, p_values))
    }

    rownames(all_padj_values) <- sampled
    rownames(all_pvalues) <- sampled
    all_padj_values <- stats::na.omit(all_padj_values)
    all_pvalues <- stats::na.omit(all_pvalues)
    num_genes <- dim(count_matrix)[1]
    colnames(all_padj_values) <- resultsNames(dds)[2:length(resultsNames(dds))]
    colnames(all_pvalues) <- resultsNames(dds)[2:length(resultsNames(dds))]
    levels_of_condition <- length(levels(condition))
    pvals_condition <- as.data.frame(
        all_pvalues[, seq_len(levels_of_condition - 1)])
    colnames(pvals_condition) <- resultsNames(dds)[2:levels_of_condition]
    rownames(pvals_condition) <- rownames(all_pvalues)
    adj_pvals_condition <- as.data.frame(
        all_padj_values[, seq_len(levels_of_condition - 1)])
    colnames(adj_pvals_condition) <-
        resultsNames(dds)[2:levels_of_condition]
    rownames(adj_pvals_condition) <- rownames(all_padj_values)
    threshold <- 0.001 * num_genes
    recommendation <- nb_proportion(adj_pvals_condition, considered_significant,
        threshold, num_samples, small_sample_cutoff, method = "DESeq2")
    res_histogram <- nb_histogram(all_pvalues) #all_padj_values)
    reference <- paste0("Paper Reference: Li, Y., ",
        "Ge, X., Peng, F. et al. Exaggerated false positives by popular ",
        "differential expression methods when analyzing human population ",
        "samples. Genome Biol 23, 79 (2022). ",
        "https://doi.org/10.1186/s13059-022-02648-4")
    return(list(recommendation = recommendation,
        res_histogram = res_histogram, reference = reference))
}

#' This function calculated the goodness of fit of edgeR for small sample
#' sizes (intended for less than or equal to 20 samples).
#' @import edgeR
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @param num_samples total number of samples to analyze
#' @param small_sample_cutoff value at which non-parametric test was used
#'   (considered "large sample size") vs parametric was used (considered
#'   "small sample size")
#' @usage edgeR_small_size(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula,
#'     num_samples,
#'     small_sample_cutoff)
#' @return a list containing the string recommendation, the histogram and a
#'   reference for the original source of the test
edgeR_small_size <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula, num_samples, small_sample_cutoff) {
    y <- DGEList(counts = count_matrix, group = condition)
    keep <- filterByExpr(y)
    y <- y[keep, , keep.lib.sizes = FALSE]
    y <- calcNormFactors(y)
    count_norm <- edgeR::cpm(y)
    if (is.null(other_variables)) {
        design <- model.matrix(~condition)
    }else {
        y$samples <- cbind.data.frame(y$samples, conditions_df)
        design <- model.matrix(as.formula(
            paste0("~ condition", model_formula)), data = y$sample)
    }
    y <- estimateDisp(y, design)

    # The size parameters and mu estimated by edgeR for each gene
    size <- 1 / y$tagwise.dispersion
    sizefactor <- y$samples$norm.factors
    libsize.factor <- mean(colSums(count_matrix)) / mean(colSums(count_norm))
    cpm.mean <- rowMeans(count_norm)

    mu_matrix <- cpm.mean %*% t(sizefactor) * libsize.factor

    # For each condition level, get goodness-of-fit p-values for each genes
    all_pvalues <- pvalues_all_genes(condition, size, mu_matrix, y$counts)

    recommendation <- nb_proportion(all_pvalues, 0.01, 0.01, num_samples,
        small_sample_cutoff, method = "edgeR")
    res_histogram <- nb_histogram(all_pvalues)
    reference <- paste0("Adapted for small sample sizes from: Li, Y., ",
        "Ge, X., Peng, F. et al. Exaggerated false positives by popular ",
        "differential expression methods when analyzing human population ",
        "samples. Genome Biol 23, 79 (2022). ",
        "https://doi.org/10.1186/s13059-022-02648-4")
    return(list(recommendation = recommendation, res_histogram = res_histogram,
        reference = reference))
}

#' This function calculated the goodness of fit of edgeR for larger sample
#' sizes (intended for more than 150 samples).
#' @import edgeR
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom stats na.omit
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @param num_samples total number of samples to analyze
#' @param sampled the down sampled matrix
#' @param small_sample_cutoff value at which non-parametric test was used
#'   (considered "large sample size") vs parametric was used (considered
#'   "small sample size")
#' @usage edgeR_large_analysis(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula,
#'     num_samples,
#'     sampled,
#'     small_sample_cutoff)
#' @return a list containing the string recommendation

edgeR_large_analysis <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula, num_samples,
    sampled, small_sample_cutoff) {
    considered_significant <- 0.05
    fit <- permuted_edgeR(count_matrix, condition, other_variables,
        conditions_df, model_formula)
    qlf <- glmQLFTest(fit, coef = 2)
    qlf_i <- topTags(qlf, n = nrow(count_matrix), p.value = 1)@.Data[[1]]
    # count the number of DEGs
    num_DEGs <- sum(qlf_i$FDR <= considered_significant)
    all_padj_values <- NULL
    all_pvalues <- NULL
    for (i in 2:length(colnames(fit$coefficients))){
        qlf <- glmQLFTest(fit, coef = colnames(fit$coefficients)[i])
        qlf_i <- topTags(qlf, n = nrow(count_matrix), p.value = 1)@.Data[[1]]
        padj_values <- as.data.frame(qlf_i$FDR, row.names = sampled)
        all_padj_values <- as.data.frame(c(all_padj_values, padj_values))
        p_values <- as.data.frame(qlf_i$PValue, row.names = sampled)
        all_pvalues <- as.data.frame(c(all_pvalues, p_values))
    }
    rownames(all_padj_values) <- sampled
    rownames(all_pvalues) <- sampled
    all_padj_values <- stats::na.omit(all_padj_values)
    all_pvalues <- stats::na.omit(all_pvalues)
    num_genes <- dim(count_matrix)[1]
    colnames(all_padj_values) <- colnames(fit$coefficients)[-1]
    colnames(all_pvalues) <- colnames(fit$coefficients)[-1]
    levels_of_condition <- length(levels(condition))
    pvals_condition <- as.data.frame(
        all_pvalues[, seq_len(levels_of_condition - 1)])
    colnames(pvals_condition) <-
        colnames(fit$coefficients)[2:levels_of_condition]
    rownames(pvals_condition) <- rownames(all_pvalues)
    adj_pvals_condition <- as.data.frame(
        all_padj_values[, seq_len(levels_of_condition - 1)])
    colnames(adj_pvals_condition) <-
        colnames(fit$coefficients)[2:levels_of_condition]
    rownames(adj_pvals_condition) <- rownames(all_padj_values)
    threshold <- 0.001 * num_genes
    recommendation <- nb_proportion(adj_pvals_condition, considered_significant,
        threshold, num_samples, small_sample_cutoff, method = "edgeR")
    res_histogram <- nb_histogram(all_pvalues) #all_padj_values)
    reference <- paste0("Paper Reference: Li, Y., ",
        "Ge, X., Peng, F. et al. Exaggerated false positives by popular ",
        "differential expression methods when analyzing human population ",
        "samples. Genome Biol 23, 79 (2022). ",
        "https://doi.org/10.1186/s13059-022-02648-4")
    return(list(recommendation = recommendation,
        res_histogram = res_histogram, reference = reference))
}

#' This function performs DESeq on the permuted dataset.
#' @import DESeq2
#' @import SummarizedExperiment
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @usage permuted_DESeq(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula)
#' @return a DESeq2 object

permuted_DESeq <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula) {

    conditions_perm <- sample(condition)

    # Do DE analysis on permuted data
    if (is.null(other_variables)) {
        dds <- DESeqDataSetFromMatrix(count_matrix,
            DataFrame(conditions_perm), ~ conditions_perm)
    }else {
        dds <- DESeqDataSetFromMatrix(count_matrix,
            DataFrame(conditions_perm, conditions_df),
            as.formula(paste0("~ conditions_perm", model_formula)))
    }
    dds <- DESeq(dds)
    return(dds)
}

#' This function performs edgeR on the permuted dataset
#' adjusted pvalues.
#' @import edgeR
#' @import SummarizedExperiment
#' @param count_matrix matrix containing the data to be analyzed
#' @param condition a vector containing a factor of the condition of interest
#'   (typically batch)
#' @param other_variables a vector of strings of other variables of interest
#' @param conditions_df data frame containing information for the other
#'   variables of interest (columns in order of the other_variables vector)
#' @param model_formula the stat formula to be used in the DESeq analysis
#' @usage permuted_edgeR(
#'     count_matrix,
#'     condition,
#'     other_variables,
#'     conditions_df,
#'     model_formula)
#' @return edgeR fit

permuted_edgeR <- function(count_matrix, condition, other_variables,
    conditions_df, model_formula) {

    conditions_perm <- sample(condition)

    y <- DGEList(counts = count_matrix, group = conditions_perm)
    y <- calcNormFactors(y)

    if (is.null(other_variables)) {
        design <- model.matrix(~condition)
    }else {
        y$samples <- cbind.data.frame(y$samples, conditions_df)
        design <- model.matrix(as.formula(paste0("~ condition",
            model_formula)),
            data = y$sample)
    }

    y <- estimateDisp(y, design)
    fit <- glmQLFit(y, design)

    return(fit)
}

#' This function creates a histogram from the negative binomial
#' goodness-of-fit adjusted pvalues.
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @param p_val_table table of adjusted p-values from the nb test
#' @return a histogram of the number of genes within a p-value range

nb_histogram <- function(p_val_table) {
    # tidy the data so there is a gene, condition and pval column
    p_val_table <- tibble::rownames_to_column(p_val_table, "features")
    p_val_table <- tidyr::pivot_longer(p_val_table,
        cols = 2:length(colnames(p_val_table)),
        names_to = "condition",
        values_to = "p_val")

    nb_histogram <- ggplot2::ggplot(p_val_table, aes_string(x = "p_val")) +
        xlab("p-value") +
        ggplot2::geom_histogram() +
        ggplot2::facet_grid(condition ~ .)

    return(nb_histogram)
}

#' This function determines the proportion of p-values below a specific
#' value and compares to the previously determined threshold
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @param p_val_table table of p-values from the nb test
#' @param low_pval value of the p-value cut off to use in proportion
#' @param threshold the value to compare the proportion of p-values to for data
#'   sets
#' @param num_samples the number of samples in the analysis
#' @param small_sample_cutoff value at which non-parametric test will be used
#'   (considered "large sample size") vs parametric will be used (considered
#'   "small sample size); default is 20
#' @param method string; method utilized for the parametric or non-parametric
#'   test; either "DESeq2" or "edgeR"
#' @usage nb_proportion(
#'     p_val_table,
#'     low_pval,
#'     threshold,
#'     num_samples,
#'     small_sample_cutoff,
#'     method)
#' @return a statement about whether DESeq2 is appropriate to use for analysis

nb_proportion <- function(p_val_table, low_pval, threshold, num_samples,
    small_sample_cutoff, method) {

    if (num_samples > 20 & num_samples < 150) {
        caution_statement <- paste0("These methods have not been tested on ",
        "data sets with between 21-149 samples. Thus, caution should be had ",
        "when interpretting the results. ")
    }else {
        caution_statement <- ""
    }

    if (method == "DESeq2") {
        caution_statement <- paste0("The DESeq2 method has not been fully ",
            "tested. Please interpret results with caution. ",
            caution_statement)
    }

    if (num_samples < small_sample_cutoff) {
        proportion_below_value <- mean(p_val_table < low_pval, na.rm = TRUE)
        nb_fit <- proportion_below_value < threshold

        if (nb_fit) {
            recommendation <- paste0("may use ", method, " for your analysis.")
        }else {
            recommendation <- paste0("should not use ", method,
                " for your analysis.")
        }

        commentary <- paste0(caution_statement, "With a p-value cut off of ",
            low_pval, ", ", (round(proportion_below_value, 2) * 100),
            "% of your features are below the cutoff. Thus based on a ",
            "threshold of ", threshold, ", you ", recommendation)
    } else {
        ngene_pval <- nrow(p_val_table)
        count_below_value_pval <- 0
        for (i in seq_len(nrow(p_val_table))){
            if (min(p_val_table[i, ]) < low_pval) {
                count_below_value_pval <- count_below_value_pval + 1
            }
        }
        proportion <- count_below_value_pval / ngene_pval
        nb_fit_pval <- proportion < threshold
        commentary <- paste0(caution_statement,
            commentary(nb_fit_pval, count_below_value_pval, proportion,
                low_pval, method))
    }

    return(commentary)
}


#' This function creates the commentary recommendation when there are more
#' than 20 samples.
#' @param nb_fit_pval Boolean representing if the p-val count is below threshold
#' @param count_below_value_pval number of features below p-val threshold
#' @param proportion numeric; proportion of genes below the p-value
#' @param low_pval pval threshold
#' @param method string; method utilized for the parametric or non-parametric
#'   test; either "DESeq2" or "edgeR"
#' @return a commentary string statement
#'
commentary <- function(nb_fit_pval, count_below_value_pval, proportion,
    low_pval, method) {
    if (nb_fit_pval) {
        if (count_below_value_pval == 0) {
            recommendation <- paste0("you may use ", method,
                " for your analysis.")
        }else {
            recommendation <- paste0("you should be cautious about using ",
                method, " for your analysis. You have significant features,",
                " and thus you are at risk of receiving false results.")
        }
    }else {
        recommendation <- paste0(
            "we do not recommend that you should use ", method,
            " for your analysis.")
    }

    commentary <- paste0("With an FDR adjusted p-value cut off of ", low_pval,
        ", ", count_below_value_pval, " of your condition variable features ",
        "are below the cutoff. If ", method, "'s assumptions are met, we would",
        " expect a uniform distribution of significant features. Since ",
        round(proportion, 2) * 100, "% of features have a significant adjusted",
        " pvalue (<", low_pval, "), ", recommendation)

    return(commentary)
}
