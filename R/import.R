# Functions for importing data
library(SummarizedExperiment)
library(dplyr)
library(tidyr)

ingest_data <- function(counts_path, metadata_path){
  # Read in counts (assuming first column is index)
  counts <- read.csv(counts_path, row.names=1)
  # Read in metadata
  md <- read.csv(metadata_path, row.names='Sample')
  # CHECK that "Sample" and "Batch" columns are in md
  ## ^^ Is this something that the SE handles?

  # Ingest into SummarizedExperiment
  se <- SummarizedExperiment(list(counts=counts), colData=md)
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
