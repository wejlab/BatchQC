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


