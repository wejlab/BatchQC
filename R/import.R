# Functions for importing data
library(SummarizedExperiment)

ingest_data <- function(counts_path, metadata_path){
  # Read in counts (assuming first column is index)
  counts <- read.csv(counts_path, row.names=1)
  # Read in metadata
  md <- read.csv(metadata_path)
  # Ingest into SummarizedExperiment
  ### THIS ISN'T COMPLETE RIGHT!
  se <- SummarizedExperiment(counts, colData=md)
}
