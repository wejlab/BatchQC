#' Preprocess assay data
#' @param se Summarized Experiment object
#' @param assay Assay from SummarizedExperiment object
#' @param nfeature Number of variable features to use
#' @param log_option "True" if data should be logged, "False" otherwise
#' @return Returns processed data

preprocess <- function(se, assay, nfeature, log_option) {

    data <- assays(se)[[assay]]
    data <- as.matrix(data)
    data <- apply(data, c(1, 2), as.numeric)
    data <- data[rowSums(data) != 0, ]

    #Finding genes w/Highest variance
    vargenes <- apply(data, 1, stats::var)
    vargenes <- vargenes[order(vargenes, decreasing = TRUE)]
    # selects top n most variable genes
    vargenes <- vargenes[seq(1, nfeature)]

    if (log_option) {
        data <- log(data + 1)
    }
    data <- data[names(vargenes), ]
    data <- data + 1

    return(data)
}
