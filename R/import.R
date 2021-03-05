#' This function allows you to create aa summarized experiment object
#' @param counts_path path to counts file
#' @param metadata_path path to metadata file
#' @return a summarized experiment object
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
summarize_experiment = function(Counts_path,metadata_path) {
  #require(reader)
  coldata <- read.table(metadata_path,header = T,row.names = 1,check.names = F,sep = get.delim(metadata_path,n = 10,delims = c('\t',',')))
  counts <-read.table(Counts_path,header = T,row.names = 1,check.names = F,sep = get.delim(Counts_path,n = 10,delims = c('\t',',')))
  counts <- counts[rowSums(counts)>0,]
  mutual_sample <- intersect(colnames(counts),rownames(coldata))
  counts <- counts[,mutual_sample]
  coldata <- coldata[mutual_sample,]

  # Normalize data
  DESEQ_normalization <- GetNormalizedMat(counts, MedianNorm(counts))
  CPM_Normalization <- (counts+1) / counts *(10^6)

  se <- SummarizedExperiment(assay=list(counts=counts,
                                        DESEQ_normalization=DESEQ_normalization,
                                        CPM_Normalization=CPM_Normalization
  ), colData=coldata)

  # Normalize data
  se@assays@data$DESEQ_normalization=GetNormalizedMat(counts, MedianNorm(counts))
  return(se)
}


#' This function allows you to ingest data
#' @param se summarized experiment
#' @param group biological covariate
#' @param batch batch
#' @import SummarizedExperiment
#' @import EBSeq
#' @return a summarized experiment object
#'
#' @export
ingest_data <- function(se,group,batch){
  #require(SummarizedExperiment)
  #require(EBSeq)

  if (!is.null(se)) {
    variables <- colnames(colData(se))
    covs <- variables[!variables%in%c(batch,group)]
    colnames(colData(se))[colnames(colData(se))==batch] <- 'Batch'

    # Add covariates
    metadata(se)$covariates <- covs
    # Add experimental group variable
    metadata(se)$Experimental_group <- group

    # Get counfounding metrics
    metadata(se)$confound.metrics <- confound_metrics(se)
    # Calculate CPM normalization for the summarizeexperiment
    #se@assays@data$CPM=((se@assays@data$counts+1)/colSums(se@assays@data$counts))*(10^6)
    # Calculate Median of Ratio normalization for the summarizeexperiment
    #require(EBSeq)
    se@assays@data$DESEQ_Method <- GetNormalizedMat(se@assays@data$counts, MedianNorm(se@assays@data$counts))
    # EdgeR won't go straight-forward on how exactly they do their normalization, so I will just pass here.
    colData(se)$library_size <- colSums(se@assays@data$counts)

  }
  else {
	se <- NULL
  }
  return(se)
}




