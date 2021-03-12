#' Returns a list of explained variation by batch and condition
#' combinations
#'
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @return List of explained variation by batch and condition
#' @export
batchqc_explained_variation <- function(se, batch, condition, assay_name) {
  df <- se@colData
  nlb <- n_distinct(as.data.frame(df[batch]))
  nlc <- n_distinct(as.data.frame(df[condition]))
  if ((nlb <= 1)&&(nlc <= 1))  {
    cond_mod <- matrix(rep(1, ncol(se)), ncol = 1)
    batch_mod <- matrix(rep(1, ncol(se)), ncol = 1)
  } else if(nlb <= 1)  {
    cond_mod <- model.matrix(~df[[condition]])
    batch_mod <- matrix(rep(1, ncol(se)), ncol = 1)
  } else if(nlc <= 1)  {
    cond_mod <- matrix(rep(1, ncol(se)), ncol = 1)
    batch_mod <- model.matrix(~df[[batch]])
  } else {
    cond_mod <- model.matrix(~df[[condition]])
    batch_mod <- model.matrix(~df[[batch]])
  }
  mod <- cbind(cond_mod, batch_mod[, -1])

  if(qr(mod)$rank<ncol(mod)){
    if(ncol(mod)==(nlb+1)){stop("The covariate is confounded with batch! Please choose a different covariate.")}
    if(ncol(mod)>(nlb+1)){
      if((qr(mod[,-c(1:nlb)])$rank<ncol(mod[,-c(1:nlb)]))){stop('The covariate is confounded with batch! Please choose a different covariate.')
      }else{stop("The covariate is confounded with batch! Please choose a different covariate.")}}
  }

  cond_test <- batchqc_f.pvalue(se, mod, batch_mod, assay_name)
  batch_test <- batchqc_f.pvalue(se, mod, cond_mod, assay_name)

  cond_ps <- cond_test$p
  batch_ps <- batch_test$p

  r2_full <- cond_test$r2_full
  cond_r2 <- batch_test$r2_reduced
  batch_r2 <- cond_test$r2_reduced
  explained_variation <- round(cbind(`Full (Condition+Batch)` = r2_full,
                                     Condition = cond_r2, Batch = batch_r2), 5) * 100
  rownames(explained_variation) <- rownames(data.matrix)
  batchqc_ev <- list(explained_variation = explained_variation,
                     cond_test = cond_test, batch_test = batch_test)

  return(batchqc_ev)
}

#' Returns R2 values from F-test (full/reduced model)
#'
#' @param se Summarized experiment object
#' @param mod mod
#' @param batch_mod mod
#' @param assay_name Name of chosen assay
#' @return List of explained variation by batch and condition
#' @export
batchqc_f.pvalue <- function(se, mod, batch_mod,assay_name) {
  mod00 <- matrix(rep(1, ncol(se)), ncol = 1)
  n <- dim(se)[2]
  m <- dim(se)[1]
  df1 <- dim(mod)[2]
  df0 <- dim(batch_mod)[2]
  p <- rep(0, m)

  resid <- as.matrix(se@assays@data[[assay_name]]) - as.matrix(se@assays@data[[assay_name]]) %*% mod %*% solve(t(mod) %*% mod) %*% t(mod)
  rss1 <- rowSums(resid * resid)
  rm(resid)

  resid0 <- as.matrix(se@assays@data[[assay_name]]) - as.matrix(se@assays@data[[assay_name]]) %*% batch_mod %*% solve(t(batch_mod) %*% batch_mod) %*% t(batch_mod)
  rss0 <- rowSums(resid0 * resid0)
  rm(resid0)

  resid00 <- as.matrix(se@assays@data[[assay_name]]) - as.matrix(se@assays@data[[assay_name]]) %*% mod00 %*% solve(t(mod00) %*% mod00) %*% t(mod00)
  rss00 <- rowSums(resid00 * resid00)
  rm(resid00)

  r2_full <- 1 - rss1/rss00
  r2_reduced <- 1 - rss0/rss00

  p <- 1
  if (df1 > df0)  {
    fstats <- ((rss0 - rss1)/(df1 - df0))/(rss1/(n - df1))
    p <- 1 - pf(fstats, df1 = (df1 - df0), df2 = (n - df1))
  }
  return(list(p = p, r2_full = r2_full, r2_reduced = r2_reduced))
}


#' Returns table with percent variation explained for specified number of genes
#'
#' @param se Summarized experiment object
#' @param mod mod
#' @param batch_mod mod
#' @param assay_name Name of chosen assay
#' @param number_of_genes Number of genes to include in the table
#' @return List of explained variation by batch and condition
#' @export
batchqc_ev_table <- function(se, batch, condition, assay_name, number_of_genes) {
  batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
  EV_table <- batchqc_ev$explained_variation[1:number_of_genes,]
  return(list(EV_table=EV_table))
}


#' Returns list of covariates not confounded by batch
#'
#' @param se Summarized experiment object
#' @param batch Batch variable
#' @return List of explained variation by batch and condition
#' @export
covariates_not_confounded <- function(se, batch) {
  df <- confoundMetrics(se,batch)
  covariate_options <- rownames(df)
  for (i in 1:dim(df)[1]) {
    if (df[i]== 1) {
      covariate_options <- covariate_options[!(covariate_options) %in% rownames(df)[i]]
    }
  }
  return(covariate_options)
}
