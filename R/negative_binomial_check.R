
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
#' @param se the se object where all the data is contained
#' @param count_matrix name of the assay with gene expression matrix (in counts)
#' @param condition name of the se colData with the condition status
#' @param batch name of the se colData containing batch information
#' @return a matrix of pvalues where each row is a gene and each column is a
#'   level within the condition of interest
#' @export
#' @examples
#' # example code
#' library(scran)
#' se <- mockSCE(ncells = 20)
#' nb_results <- goodness_of_fit_DESeq2(se = se, count_matrix = "counts",
#'   condition = "Treatment", batch = "Mutation_Status")
#' nb_results

goodness_of_fit_DESeq2 <- function(se, count_matrix, condition, batch) {
    # Obtain needed data from se object
    count_matrix <- SummarizedExperiment::assays(se)[[count_matrix]]
    condition <- SummarizedExperiment::colData(se)[[condition]]
    batch <- SummarizedExperiment::colData(se)[[batch]]

    if (dim(count_matrix)[2] > 20) {
        stop("This goodness of fit test should only be used for data sets with
            20 or fewer samples.")
    }else {
        # Use DESeq2 to fit the NB model
        if (length(unique(batch)) == 1) {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                S4Vectors::DataFrame(condition, batch), ~ condition)
        }else {
            dds <- DESeqDataSetFromMatrix(count_matrix,
                S4Vectors::DataFrame(condition, batch), ~ condition + batch)
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
        return(all_pvalues)
    }

}

#' This function creates a histogram from the negative binomial goodness-of-fit
#' pvalues.
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @param p_val_table table of p-values from the nb test
#' @return a histogram of the number of genes within a p-value range
#' @export
#' @examples
#' # example code
#' library(scran)
#' se <- mockSCE(ncells = 20)
#' nb_results <- goodness_of_fit_DESeq2(se = se, count_matrix = "counts",
#'   condition = "Treatment", batch = "Mutation_Status")
#' nb_histogram(nb_results)
nb_histogram <- function(p_val_table) {
    # tidy the data so there is a gene, condition and pval column
    p_val_table <- tibble::rownames_to_column(p_val_table, "features")
    p_val_table <- tidyr::pivot_longer(p_val_table,
        cols = 2:length(colnames(p_val_table)),
        names_to = "condition",
        values_to = "p_val")

    nb_histogram <- ggplot2::ggplot(p_val_table, aes_string(x = "p_val")) +
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
#' @param threshold the value to compare the p-values to
#' @return a statement about whether DESeq2 is appropriate to use for analysis
#' @export
#' @examples
#' # example code
#' library(scran)
#' se <- mockSCE(ncells = 20)
#' nb_results <- goodness_of_fit_DESeq2(se = se, count_matrix = "counts",
#'   condition = "Treatment", batch = "Mutation_Status")
#' nb_proportion(nb_results, low_pval = 0.01, threshold = 0.42)

nb_proportion <- function(p_val_table, low_pval = 0.01, threshold = 0.42) {
    proportion_below_value <- mean(p_val_table < low_pval, na.rm = TRUE)
    nb_fit <- proportion_below_value < threshold

    if (nb_fit) {
        recommendation <- "can use DESeq2 for your analysis."
    }else {
        recommendation <- "should not use DESeq2 for your analysis."
    }

    commentary <- paste0("With a p-value cut off of ", low_pval, ", ",
        (round(proportion_below_value, 2) * 100),
        "% of your features are below the cutoff. ",
        "Thus based on a threshold of ",
        threshold, ", you ", recommendation)
    return(commentary)
}
