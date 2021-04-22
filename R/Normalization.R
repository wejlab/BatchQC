#' This function allows you to Add normalized count matrix to the se object
#' @param se SummarizeExperiment
#' @param Method Normalization Method
#' @param assaytouse Which assay use to do normalization
#' @param output_assay_name name of results assay
#' @return a summarized experiment object with normalized assay appended.
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
NormalizateSE = function(se,Method,assaytouse,output_assay_name) {
  se=se
  if (Method=='CPM') {
    se@assays@data[[output_assay_name]]=(se@assays@data[[assaytouse]]+1) / colSums(se@assays@data[[assaytouse]]) *(10^6)

  }
  else if (Method=='DESeq') {
    se@assays@data[[output_assay_name]]=GetNormalizedMat(se@assays@data[[assaytouse]],
                                                         MedianNorm(se@assays@data[[assaytouse]]))

  }
  return(se)
}
