# Functions for importing data
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
  # CHECK that "Sample" and "Batch" columns are in md
  ## ^^ Is this something that the SE handles?

  # Add in check of integrity: only create se object if all the samples in the metadata are presented in counts and vice versa, return NULL object else and capture the error later.
  if (all(rownames(md)%in%colnames(counts))&all(colnames(counts)%in%rownames(md))) {
  # Order the columns of the count data in the order of samples in metadata.
    counts = counts[,match(rownames(md),colnames(counts))]
    se <- SummarizedExperiment(list(counts=counts), colData=md)
  }
  else {
	se = NULL
  }
  #
  # Ingest into SummarizedExperiment
  return(se)
}

batch_design <- function(se, covariate){
  # Create a batch design table for the provided covariate
  design <- colData(se) %>% as_tibble %>% group_by(eval(as.symbol(covariate))) %>% count(Batch) %>% pivot_wider(names_from = Batch, values_from = n)
  names(design)[names(design) == "eval(as.symbol(covariate))"] <- ""
  for (i in 2:length(design)) {
    colnames(design)[i] <- paste("Batch",i-1)
  }
  return(design)
}

std_pearson_corr_coef <- function(se, covariate) {
  # Calculate standardized Pearson correlation coefficient

}

cramers_v <- function(se, covariate) {
  # Calculate Cramer's V

}
