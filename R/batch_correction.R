#' Batch Correct
#' This function allows you to Add batch corrected count matrix to the se object
#' @param se SummarizeExperiment
#' @param method Normalization Method
#' @param assay_to_normalize Which assay use to do normalization
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
batch_correct = function(se, method, assay_to_normalize, batch, group=NULL,
                         covar, output_assay_name) {
  se=se
  batch=data.frame(colData(se))[,batch]

  if (method=='ComBat-Seq'){
  if (is.null(covar)) {
      se@assays@data[[output_assay_name]] = ComBat_seq(as.matrix(
        se@assays@data[[assay_to_normalize]]), batch = batch)
    }
    else {
      if (length(covar)==1) {
        cov=data.frame(colData(se))[,covar]
        cov=as.factor(cov)
        cov=as.numeric(cov)
        cov=as.matrix(cov)
        rownames(cov)=rownames(data.frame(colData(se)))
        if (!is.null(group)){
        se@assays@data[[output_assay_name]] = ComBat_seq(as.matrix(
          se@assays@data[[assay_to_normalize]]), batch = batch, covar_mod = cov,
          group = group, full_mod = TRUE)
        }
        else {
          se@assays@data[[output_assay_name]] = ComBat_seq(as.matrix(
            se@assays@data[[assay_to_normalize]]), batch = batch,
            covar_mod = cov, group = group)
        }
      }
      else {
        cov=data.frame(colData(se))[,covar]
        for (i in seq_len(ncol(cov))) {
          cov[,i]=as.factor(cov[,i])
          cov[,i]=as.numeric(cov[,i])
        }

        if (!is.null(group)){
          se@assays@data[[output_assay_name]] = ComBat_seq(as.matrix(
            se@assays@data[[assay_to_normalize]]), batch = batch,
            covar_mod = cov, group = group, full_mod = TRUE)
        }
        else {
          se@assays@data[[output_assay_name]] = ComBat_seq(as.matrix(
            se@assays@data[[assay_to_normalize]]), batch = batch,
            covar_mod = cov, group = group)

        }
      }
    }
  }
  else if (method=='ComBat') {
    if (is.null(covar)) {
      se@assays@data[[output_assay_name]] =
        ComBat(dat = se@assays@data[[assay_to_normalize]], batch = batch)
    }
    else {
      if (length(covar)==1) {
        cov=data.frame(colData(se))[,covar]
        cov=as.factor(cov)
        cov=as.numeric(cov)
        cov=data.frame(cov)
        colnames(cov)=covar
        rownames(cov)=rownames(data.frame(colData(se)))

        model = stats::model.matrix(stats::as.formula(
          paste0('~',colnames(cov))), data = cov)
        results = ComBat(dat = se@assays@data[[assay_to_normalize]],
                       batch = batch,mod = model)
        results[is.na(results)] <- 0
        se@assays@data[[output_assay_name]]=results
      }
      else {
        cov=data.frame(colData(se))[,covar]
        for (i in seq_len(ncol(cov))) {
          cov[,i]=as.factor(cov[,i])
          cov[,i]=as.numeric(cov[,i])
        }
        cov=data.frame(cov)
        rownames(cov)=rownames(data.frame(colData(se)))
        colnames(cov)=covar
        linearmodel=as.function(paste0('~',paste(colnames(cov),sep = '+')))
        model = stats::model.matrix(linearmodel,data = cov)

        results = ComBat(dat = se@assays@data[[assay_to_normalize]],
                       batch = batch,mod = model)
        results[is.na(results)] <- 0
        se@assays@data[[output_assay_name]]=results

      }
    }
  }
  return(se)
}
