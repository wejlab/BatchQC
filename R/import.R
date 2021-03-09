#' This function allows you to create a summarized experiment object
#' @param counts_path path to counts file
#' @param metadata_path path to metadata file
#' @return a summarized experiment object
#' @import SummarizedExperiment
#' @import reader
#' @import EBSeq
#'
#' @export
summarized_experiment = function(Counts_path,metadata_path) {
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
  # Add library size
  colData(se)$library_size <- colSums(se@assays@data$counts)
  return(se)
}
