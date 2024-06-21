
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
#' @param count_matrix name of the se assay with gene expression matrix (in counts)
#' @param condition name of the se colData with the condition status
#' @param batch name of the se colData containing batch information
#' @return a matrix of pvalues where each row is a gene and each column is a
#'   level within the condition of interest
#' @export
#' @examples
#' # example code
#' library(scran)
#' se <- mockSCE()
#' nb_results <- goodness_of_fit_DESeq2(se = se, count_matrix = "counts",
#'   condition = "Treatment", batch = "Mutation_Status")
#' nb_results

goodness_of_fit_DESeq2 <- function(se, count_matrix, condition, batch) {
    # Obtain needed data from se object
    count_matrix <- SummarizedExperiment::assays(se)[[count_matrix]]
    condition <- SummarizedExperiment::colData(se)[[condition]]
    batch <- SummarizedExperiment::colData(se)[[batch]]

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
