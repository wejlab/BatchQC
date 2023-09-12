#' This function allows you to add normalized count matrix to the SE object
#' @param se SummarizedExperiment Object
#' @param method Normalization Method, either 'CPM' or 'DESeq'
#' @param assay_to_normalize Which SE assay to do normalization on
#' @param output_assay_name name for the resulting normalized assay
#' @return the original SE object with normalized assay appended
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se_CPM_normalized <- BatchQC::normalize_SE(se, method = "CPM",
#'                                 assay_to_normalize = "counts",
#'                                 output_assay_name =
#'                                     "CPM_normalized_counts")
#' se_DESeq_normalized <- BatchQC::normalize_SE(se, method = "DESeq",
#'                                 assay_to_normalize = "counts",
#'                                 output_assay_name =
#'                                     "DESeq_normalized_counts")
#' se_CPM_normalized
#' se_DESeq_normalized
#'
#' @export
normalize_SE <- function(se, method, assay_to_normalize, output_assay_name) {
    se <- se
    if (method == 'CPM') {
        assays(se)[[output_assay_name]] <-
            (assays(se)[[assay_to_normalize]] + 1) /
            colSums(assays(se)[[assay_to_normalize]]) * (10^6)

    }else if (method == 'DESeq') {
        assays(se)[[output_assay_name]] <- GetNormalizedMat(
            assays(se)[[assay_to_normalize]],
            MedianNorm(assays(se)[[assay_to_normalize]]))

    }
    return(se)
}
