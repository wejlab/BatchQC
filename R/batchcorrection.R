#' @param se SummarizeExperiment
#' @param Method Normalization Method
#' @param assaytouse Which assay use to do normalization
#' @param batch The batch variable
#' @param covar Covariate Matrix

#' @param output_assay_name name of results assay
#' @return a summarized experiment object with normalized assay appended.
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
BatchCorrect = function(se,Method,assaytouse,batch,covar,output_assay_name) {
  se=se
  batch=data.frame(colData(se))[,batch]
  if (Method=='ComBat-Seq'){
    tryCatch(  {if (is.null(covar)) {
      se@assays@data[[output_assay_name]]=ComBat_seq(se@assays@data[[assaytouse]],batch = batch)

    }
    else {
      if (length(covar)==1) {
        cov=data.frame(colData(se))[,covar]
        cov=as.factor(cov)
        cov=as.numeric(cov)
        cov=as.matrix(cov)
        rownames(cov)=rownames(data.frame(colData(se)))
        se@assays@data[[output_assay_name]]=ComBat_seq(se@assays@data[[assaytouse]],batch = batch,covar_mod = cov)

      }
      else {
        cov=data.frame(colData(se))[,covar]
        for (i in 1:ncol(cov)) {
          cov[,i]=as.factor(cov[,i])
          cov[,i]=as.numeric(cov[,i])
        }

        se@assays@data[[output_assay_name]]=ComBat_seq(se@assays@data[[assaytouse]],batch = batch,covar_mod = as.matrix(cov))


      }

    }},
    error=function(cond) {
      stop(cond)
    })


  }
  return(se)
}
