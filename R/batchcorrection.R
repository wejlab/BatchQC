#' This function allows you to Add batch corrected count matrix to the se object
#' @param se SummarizeExperiment
#' @param Method Normalization Method
#' @param assaytouse Which assay use to do normalization
#' @param batch The batch
#' @param group The group variable
#' @param covar Covariate Matrix
#' @param output_assay_name name of results assay
#' @return a summarized experiment object with normalized assay appended.
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#' @import sva
#'
#' @export
BatchCorrect = function(se,Method,assaytouse,batch,group=NULL,covar,output_assay_name) {
  se=se
  batch=data.frame(colData(se))[,batch]
  if (Method=='ComBat-Seq'){
  if (is.null(covar)) {
      se@assays@data[[output_assay_name]]=ComBat_seq(as.matrix(se@assays@data[[assaytouse]]),batch = batch)

    }
    else {
      if (length(covar)==1) {
        cov=data.frame(colData(se))[,covar]
        cov=as.factor(cov)
        cov=as.numeric(cov)
        cov=as.matrix(cov)
        rownames(cov)=rownames(data.frame(colData(se)))
        if (!is.null(group)){
        se@assays@data[[output_assay_name]]=ComBat_seq(as.matrix(se@assays@data[[assaytouse]]),batch = batch,covar_mod = cov,group = group,full_mod = T)
        }
        else {
          se@assays@data[[output_assay_name]]=ComBat_seq(as.matrix(se@assays@data[[assaytouse]]),batch = batch,covar_mod = cov,group = group)

        }
      }
      else {
        cov=data.frame(colData(se))[,covar]
        for (i in 1:ncol(cov)) {
          cov[,i]=as.factor(cov[,i])
          cov[,i]=as.numeric(cov[,i])
        }

        if (!is.null(group)){
          se@assays@data[[output_assay_name]]=ComBat_seq(as.matrix(se@assays@data[[assaytouse]]),batch = batch,covar_mod = cov,group = group,full_mod = T)
        }
        else {
          se@assays@data[[output_assay_name]]=ComBat_seq(as.matrix(se@assays@data[[assaytouse]]),batch = batch,covar_mod = cov,group = group)

        }

      }

    }


  }
  else if (Method=='ComBat') {
    if (is.null(covar)) {
      se@assays@data[[output_assay_name]]=ComBat(dat = se@assays@data[[assaytouse]],
                                                 batch = batch)

    }
    else {
      if (length(covar)==1) {
        cov=data.frame(colData(se))[,covar]
        cov=as.factor(cov)
        cov=as.numeric(cov)
        cov=as.matrix(cov)
        rownames(cov)=rownames(data.frame(colData(se)))
        se@assays@data[[output_assay_name]]=ComBat(dat = se@assays@data[[assaytouse]],
                                                   batch = batch,mod = cov)
      }
      else {
        cov=data.frame(colData(se))[,covar]
        for (i in 1:ncol(cov)) {
          cov[,i]=as.factor(cov[,i])
          cov[,i]=as.numeric(cov[,i])
        }
        cov=as.matrix(cov)
        se@assays@data[[output_assay_name]]=ComBat(dat = se@assays@data[[assaytouse]],
                                                   batch = batch,mod = cov)

      }
    }
  }
  return(se)
}
