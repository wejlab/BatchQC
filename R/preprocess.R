#' Preprocess assay data
#' @param se Summarized Experiment object
#' @param assay Assay from SummarizedExperiment object
#' @param nfeature Number of variable features to use
#' @return Returns processed data

preprocess <- function(se, assay, nfeature) {

    data <- assays(se)[[assay]]
    data <- as.matrix(data)
    data <- apply(data, c(1, 2), as.numeric)
    data <- data[rowSums(data) != 0, ]

    vargenes <- apply(data, 1, stats::var)
    vargenes <- vargenes[order(vargenes, decreasing = TRUE)]
    vargenes <- vargenes[seq(1, nfeature)]

    data <- log(data + 1)
    data <- data[names(vargenes), ]
    data <- data + 1

    return(data)
}
