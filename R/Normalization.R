#' This function allows you to add normalized count matrix to the SE object
#' @param se SummarizedExperiment Object
#' @param method Normalization Method, either 'CPM' or 'DESeq' or 'none' for
#'   log only
#' @param log_bool True or False; True to log normalize the data set after
#'   normalization method
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
#'                                 log_bool = FALSE,
#'                                 assay_to_normalize = "counts",
#'                                 output_assay_name =
#'                                     "CPM_normalized_counts")
#' se_DESeq_normalized <- BatchQC::normalize_SE(se, method = "DESeq",
#'                                 log_bool = FALSE,
#'                                 assay_to_normalize = "counts",
#'                                 output_assay_name =
#'                                     "DESeq_normalized_counts")
#' se_CPM_normalized
#' se_DESeq_normalized
#'
#' @export
normalize_SE <- function(se, method, log_bool, assay_to_normalize,
    output_assay_name) {
    se <- se
    if (method == 'CPM') {
        assays(se)[[output_assay_name]] <-
            (assays(se)[[assay_to_normalize]] + 1) /
            colSums(assays(se)[[assay_to_normalize]]) * (10^6)

    }else if (method == 'DESeq') {
        assays(se)[[output_assay_name]] <- GetNormalizedMat(
            assays(se)[[assay_to_normalize]],
            MedianNorm(assays(se)[[assay_to_normalize]]))
    }else {
        assays(se)[[output_assay_name]] <- assays(se)[[assay_to_normalize]]
    }

    if (log_bool) {
        assays(se)[[output_assay_name]] <- log(assays(se)[[output_assay_name]])
    }
    return(se)
}
