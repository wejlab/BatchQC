# Functions for importing data and summarizing experimental design
library(SummarizedExperiment)
library(dplyr)
library(tidyr)

ingest_data <- function(se,group,batch){
  require(SummarizedExperiment)
  require(EBSeq)

  if (!is.null(se)) {
    variables=colnames(colData(se))
    covs=variables[!variables%in%c(batch,group)]
    colnames(colData(se))[colnames(colData(se))==batch]='Batch'

    # Add covariates
    metadata(se)$covariates <- covs
    # Add experimental group variable
    metadata(se)$Experimental_group=group

    # Get counfounding metrics
    metadata(se)$confound.metrics <- confound_metrics(se)
    # Calculate CPM normalization for the summarizeexperiment
    se@assays@data$CPM=((se@assays@data$counts+1)/colSums(se@assays@data$counts))*(10^6)
    # Calculate Median of Ratio normalization for the summarizeexperiment
    require(EBSeq)
    se@assays@data$DESEQ_Method=GetNormalizedMat(se@assays@data$counts, MedianNorm(se@assays@data$counts))
    # EdgeR won't go straight-forward on how exactly they do their normalization, so I will just pass here.
    colData(se)$library_size=colSums(se@assays@data$counts)

  }
  else {
	se = NULL
  }
  return(se)
}

batch_design <- function(se, covariate){
  #' Create a batch design table for the provided covariate
  design <- colData(se) %>% as_tibble %>% group_by(eval(as.symbol(covariate))) %>% count(Batch) %>% pivot_wider(names_from = Batch, values_from = n)
  names(design)[names(design) == "eval(as.symbol(covariate))"] <- ""
  for (i in 2:length(design)) {
    colnames(design)[i] <- paste("Batch",i-1)
  }
  return(design)
}

cor_props <- function(bd){
  #' Calculate correlation properties on a batch_design matrix `bd`

  # Subset matrix to design only
  m = bd[, -1, ]
  rowsums = rowSums(m)
  colsums = colSums(m)
  tablesum = sum(rowsums)
  expected = matrix(0, nrow(m), ncol(m))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      expected[i, j] = rowsums[i] * colsums[j]/tablesum
    }
  }
  chi = sum((m - expected)^2/expected)
  mmin = min(nrow(m), ncol(m))

  out = list("chi" = chi, "mmin"=mmin, "tablesum"=tablesum)
  return(out)
}

std_pearson_corr_coef <- function(bd) {
  #' Calculate standardized Pearson correlation coefficient
  c <- cor_props(bd)
  r <- sqrt(c$chi * c$mmin/((c$chi + c$tablesum) * (c$mmin - 1)))
  return(r)
}

cramers_v <- function(bd) {
  # Calculate Cramer's V
  c <- cor_props(bd)
  v <- sqrt(c$chi/(c$tablesum * (c$mmin - 1)))
  return(v)
}

confound_metrics <- function(se){
  covs = metadata(se)$covariates
  metrics <- list("Pearson Correlation Coefficient"=std_pearson_corr_coef, "Cramer's V"=cramers_v)
  metric.mat <- matrix(nrow=length(covs), ncol=length(metrics), dimnames = list(covs, names(metrics)))

  for (c in covs){
    # Get batch design
    bd <- batch_design(se, c)
    for (m in names(metrics)){
      # Compute metric and place in appropriate slot
      metric.mat[c, m] <- metrics[[m]](bd)
    }
  }
  # Add metrics to se
  return(metric.mat)
}





