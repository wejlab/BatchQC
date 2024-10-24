#' This function calculates p-values for each gene given counts, estimated NB
#' size, and estimated NB mean
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

#' This function calculates goodness-of-fit pvalues for all genes by looking at
#' how the NB model by DESeq2 fit the data
#' @import DESeq2
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom stats na.omit
#' @param se the se object where all the data is contained
#' @param count_matrix name of the assay with gene expression matrix (in counts)
#' @param condition name of the se colData with the condition status
#' @param other_variables name of the se colData containing other variables of
#'   interest that should be considered in the DESeq2 model
#' @param num_genes downsample value, default is 500 (or all genes if less)
#' @param seeding integer to set the seed to for reproducibility; default is 13
#' @return a matrix of pvalues where each row is a gene and each column is a
#'   level within the condition of interest
#' @export
#' @examples
#' # example code
#' library(scran)
#' se <- mockSCE(ncells = 20)
#' se$Treatment <- as.factor(se$Treatment)
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' nb_results <- goodness_of_fit_DESeq2(se = se, count_matrix = "counts",
#'   condition = "Treatment", other_variables = "Mutation_Status")
#' nb_results[1]
#' nb_results[2]
#' nb_results[3]


goodness_of_fit_DESeq2 <- function(se, count_matrix, condition,
    other_variables = NULL, num_genes = 500, seeding = 13) {
    # set seed
    set.seed(seeding)
    # Obtain needed data from se object
    count_matrix <- SummarizedExperiment::assays(se)[[count_matrix]]
    condition <- SummarizedExperiment::colData(se)[[condition]]
    condition <- as.factor(condition)

    num_samples <- dim(count_matrix)[2]

    # Ensure the number of genes is greater than the desired number for sampling
    if (dim(count_matrix)[1] < num_genes) {
        num_genes <- dim(count_matrix)[1]
    }

    # Down sample
    if (dim(count_matrix)[1] > num_genes) {
        sampled <- sample(row.names(count_matrix), num_genes)
        col_names_prior <- colnames(count_matrix)
        count_matrix <- count_matrix[sampled, ]
        #rownames(count_matrix) <- sampled
        #colnames(count_matrix) <- col_names_prior
    }
    conditions_df <- NULL
    formula_for_DeSeq <- ""

    if (!is.null(other_variables)) {
        for (i in 1:length(other_variables)) {
            conditions_df <- DataFrame(c(conditions_df, SummarizedExperiment::colData(se)[[other_variables[i]]]))
            formula_for_DeSeq <- paste0(formula_for_DeSeq, " + ", other_variables[i])
        }
    }

    colnames(conditions_df) <- other_variables

    for (i in 1:length(colnames(conditions_df))){
        conditions_df[, i] <- as.factor(conditions_df[, i])
    }

    if (num_samples < 20) {
        # Use DESeq2 to fit the NB model
        if (is.null(other_variables)) {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                S4Vectors::DataFrame(condition), ~ condition)
        }else {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                S4Vectors::DataFrame(condition, conditions_df),
                as.formula(paste0("~ condition", formula_for_DeSeq)))
        }

        dds <- DESeq(dds)

        # The size parameters estimated by DESeq2 for each gene
        size <- 1 / dispersions(dds)

        # The mu parameters estimated by DESeq2 for each count
        mu_matrix <- assays(dds)[["mu"]]

        # Count the number of levels in condition
        unique_conditions <- unique(condition)
        num_unique_conditions <- length(unique_conditions)

        # For each condition level, get the goodness-of-fit p-values for each genes
        all_pvalues <- sapply(seq_len(length(unique_conditions)), function(j) {
            index_j <- which(condition == unique_conditions[j])
            # For one condition level, calculate the goodness-of-fit p-values
            pvalues_level <-  sapply(seq_len(length(size)), function(i) {
                mu_gene <- mu_matrix[i, index_j]
                count_condition <- count_matrix[i, index_j]
                pvalue <- counts2pvalue(counts = count_condition, size = size[i],
                    mu = mu_gene)
                return(pvalue)
            })
            return(pvalues_level)
        })

        all_pvalues <- as.data.frame(all_pvalues, row.names =
                row.names(count_matrix))
        colnames(all_pvalues) <- unique_conditions
        recommendation <- nb_proportion(all_pvalues, 0.01, 0.42, num_samples)
        res_histogram <- nb_histogram(all_pvalues)
        reference <- "Adapted for small sample sizes from: Li, Y., Ge, X., Peng, F. et al. Exaggerated false positives by popular differential expression methods when analyzing human population samples. Genome Biol 23, 79 (2022). https://doi.org/10.1186/s13059-022-02648-4"
    }else {
        conditions_perm <- sample(condition)
        # Do DE analysis on permuted data
        if (is.null(other_variables)) {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                DataFrame(conditions_perm), ~ conditions_perm)
        }else {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                DataFrame(conditions_perm, conditions_df),
                as.formula(paste0("~ conditions_perm", formula_for_DeSeq)))
        }
        dds <- DESeq(dds)
        res <- results(dds)
        # count the number of DEGs
        num_DEGs <- sum(res$padj <= 0.05)

        all_pvalues <- NULL
        for (i in 2:length(resultsNames(dds))){
            pvalues <- as.data.frame(results(dds, name = resultsNames(dds)[i])$padj, row.names = sampled)
            all_pvalues <- as.data.frame(c(all_pvalues, pvalues))
        }
        rownames(all_pvalues) <- sampled
        all_pvalues <- stats::na.omit(all_pvalues)
        num_genes <- dim(count_matrix)[1]

        colnames(all_pvalues) <- resultsNames(dds)[2:length(resultsNames(dds))]
        levels_of_condition <- length(levels(condition))

        pvals_condition <- as.data.frame(
            all_pvalues[, 1:(levels_of_condition - 1)])
        colnames(pvals_condition) <- resultsNames(dds)[2:levels_of_condition]
    rownames(pvals_condition) <- rownames(all_pvalues)
        threshold <- floor(0.001 * num_genes)
        recommendation <- nb_proportion(pvals_condition,
            0.05,
            threshold,
            num_samples)
        res_histogram <- nb_histogram(all_pvalues)
        reference <- "Paper Reference: Li, Y., Ge, X., Peng, F. et al. Exaggerated false positives by popular differential expression methods when analyzing human population samples. Genome Biol 23, 79 (2022). https://doi.org/10.1186/s13059-022-02648-4"
    }
    return(list(recommendation = recommendation, res_histogram = res_histogram,
        reference = reference))
}

#' This function creates a histogram from the negative binomial goodness-of-fit
#' pvalues.
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @param p_val_table table of p-values from the nb test
#' @return a histogram of the number of genes within a p-value range

nb_histogram <- function(p_val_table) {
    # tidy the data so there is a gene, condition and pval column
    p_val_table <- tibble::rownames_to_column(p_val_table, "features")
    p_val_table <- tidyr::pivot_longer(p_val_table,
        cols = 2:length(colnames(p_val_table)),
        names_to = "condition",
        values_to = "p_val")

    nb_histogram <- ggplot2::ggplot(p_val_table, aes_string(x = "p_val")) +
        xlab("adjusted p-value (FDR)") +
        ggplot2::geom_histogram() +
        ggplot2::facet_grid(condition ~ .)

    return(nb_histogram)
}

#' This function determines the proportion of p-values below a specific value
#' and compares to the previously determined threshold of 0.42 for extreme low
#' values.
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @param p_val_table table of p-values from the nb test
#' @param low_pval value of the p-value cut off to use in proportion
#' @param threshold the value to compare the proportion of p-values to for data
#'   sets less than 20, default is 0.42
#' @param num_samples the number of samples in the analysis
#' @return a statement about whether DESeq2 is appropriate to use for analysis

nb_proportion <- function(p_val_table, low_pval = 0.01, threshold = 0.42, num_samples) {
    if (num_samples < 20) {
        proportion_below_value <- mean(p_val_table < low_pval, na.rm = TRUE)
        nb_fit <- proportion_below_value < threshold

        if (nb_fit) {
            recommendation <- "may use DESeq2 for your analysis."
        }else {
            recommendation <- "should not use DESeq2 for your analysis."
        }

        commentary <- paste0("With an adjusted FDR cut off of ", low_pval, ", ",
            (round(proportion_below_value, 2) * 100),
            "% of your features are below the cutoff. ",
            "Thus based on a threshold of ",
            threshold, ", you ", recommendation)
    }else {
        ngenes <- nrow(p_val_table)
        threshold <- ngenes * 1 / 1000

        count_below_value <- 0
        for (i in 1:nrow(p_val_table)){
            if (min(p_val_table[i, ]) < low_pval) {
                count_below_value <- count_below_value + 1
            }
        }

        nb_fit <- count_below_value < threshold

        if (nb_fit) {
            if (count_below_value == 0) {
                recommendation <- "may use DESeq2 for your analysis."
            }else {
                recommendation <- paste0("should be cautious about using DESeq2",
                " for your analysis. You are at risk of receiving false results.")
            }
        }else {
            recommendation <- "should not use DESeq2 for your analysis."
        }

        commentary <- paste0("With an adjusted FDR cut off of ", low_pval, ", ",
            count_below_value,
            " of your condition variable features are below the cutoff. ",
            "If DESeq's assumptions are met, we would not expect to find any",
            " significant features. Since ",
            count_below_value, " features are significant, you ",
            recommendation)
    }

    return(commentary)
}
