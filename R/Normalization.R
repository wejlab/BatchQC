#' This function allows you to add normalized count matrix to the SE object
#' @param se SummarizedExperiment Object
#' @param method Normalization Method, either 'CPM' or 'DESeq'
#' @param assay_to_normalize Which SE assay to do normalization on
#' @param output_assay_name name for the resulting normalized assay
#' @return the original SE object with normalized assay appended
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
normalize_SE <- function(se, method, assay_to_normalize, output_assay_name) {
    se <- se
    if (method == 'CPM') {
        se@assays@data[[output_assay_name]] <-
            (se@assays@data[[assay_to_normalize]] +1) /
            colSums(se@assays@data[[assay_to_normalize]])*(10^6)

    }
    else if (method == 'DESeq') {
        se@assays@data[[output_assay_name]] <- GetNormalizedMat(
            se@assays@data[[assay_to_normalize]],
            MedianNorm(se@assays@data[[assay_to_normalize]]))

    }
    return(se)
}
