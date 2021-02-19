# Functions for importing data and summarizing experimental design
library(SummarizedExperiment)
library(dplyr)
library(tidyr)

ingest_data <- function(counts_path, metadata_path){
  require(SummarizedExperiment)
  # Format for input:
  ## Counts must be a comma delimited csv file with header and sample name in the first row.
  ## Metadata is the same.
  # Read in counts (assuming first column is index)
  counts <- read.csv(counts_path, row.names=1,header=T)
  # Read in metadata
  md <- read.csv(metadata_path, row.names=1,header=T)
  # CHECK that "Sample" and "Batch" columns are in md and find covariates
  cols <- names(md)
  covs <- cols[cols != 'Batch']

  # Add in check of integrity: only create se object if all the samples in the metadata are presented in counts and vice versa, return NULL object else and capture the error later.
  if (all(rownames(md)%in%colnames(counts))&all(colnames(counts)%in%rownames(md))) {
  # Order the columns of the count data in the order of samples in metadata.
    counts = counts[,match(rownames(md),colnames(counts))]
    se <- SummarizedExperiment(list(counts=counts), colData=md)
    # Add covariates
    metadata(se)$covariates <- covs
    # Get counfounding metrics
    metadata(se)$confound.metrics <- confound_metrics(se)
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
