globalVariables(c("chosen"))

#' This function allows you to Add normalized count matrix to the SE object
#' @param se SummarizedExperiment
#' @param method Differential Expression Analysis Method
#' @param conditions Condition covariate of interest
#' @param assay_to_analyze Which assay use to do DE analysis
#' @param output_assay_name name of results assay
#' @return a summarized experiment object with normalized assay appended.
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
analyze_SE = function(se, method, conditions, assay_to_analyze,
                      output_assay_name) {
  se=se
  if (method=='wilcox') {
    output_assay_name = scran::findMarkers(se, se@colData@data$conditions,
                                           test.type=method, pval.type="all", lfc=1,
                                           subset.row=chosen)
    
  }
  else if (method=='DESeq') {
    se@assays@data[[output_assay_name]] = GetNormalizedMat(
      se@assays@data[[assay_to_analyze]],
      MedianNorm(se@assays@data[[assay_to_analyze]]))
    
  }
  return(se)
}