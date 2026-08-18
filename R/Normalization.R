#' This function allows you to add normalized count matrix to the SE object
#' @param se SummarizedExperiment Object
#' @param method string; Normalization Method, either 'CPM', 'DESeq', 'edgeR',
#'   'voom', or 'none' for log(x+1) only
#' @param log_bool True or False; True to log normalize the data set after
#'   normalization method
#' @param assay_to_normalize string; SE assay to do normalization on
#' @param output_assay_name string; name for the resulting normalized assay
#' @param condition string; the biological variable of interest, required for
#'   voom, default 'NULL'
#' @param batch string; the batch variable, required for voom, default 'NULL'
#' @usage normalize_SE(
#'     se,
#'     method,
#'     log_bool,
#'     assay_to_normalize,
#'     output_assay_name,
#'     condition = NULL,
#'     batch = NULL)
#' @return the original SE object with normalized assay appended
#' @import SummarizedExperiment
#' @import EBSeq
#' @import edgeR
#' @import reader
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
    output_assay_name, condition = NULL, batch = NULL) {
    se <- se
    if (method == 'CPM') {
        assays(se)[[output_assay_name]] <-
            (assays(se)[[assay_to_normalize]]) /
            colSums(assays(se)[[assay_to_normalize]]) * (10^6)

    }else if (method == 'DESeq') {
        assays(se)[[output_assay_name]] <- EBSeq::GetNormalizedMat(
            assays(se)[[assay_to_normalize]],
            EBSeq::MedianNorm(assays(se)[[assay_to_normalize]]))

    }else if (method == "edgeR") {
        dge <- edgeR::DGEList(counts = assays(se)[[assay_to_normalize]])
        dge <- edgeR::normLibSizes(dge)
        assays(se)[[output_assay_name]] <- dge$counts

    }else if (method == "voom") {
        if (is.null(condition) || is.null(batch)) {
            warning("You must provide the condition and batch variables.")
            return()
        }else {
            condition <- colData(se)[[condition]]
            batch <- colData(se)[[batch]]
            design <- model.matrix(~condition + batch)
            voom_result <- voom(counts = assays(se)[[assay_to_normalize]],
                design)
            assays(se)[[output_assay_name]] <- voom_result$E
        }
    }else {
        assays(se)[[output_assay_name]] <- assays(se)[[assay_to_normalize]]
    }

    if (log_bool) {
        assays(se)[[output_assay_name]] <-
            log(assays(se)[[output_assay_name]] + 1)
    }
    return(se)
}
