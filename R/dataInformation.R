#' Batch and Condition indicator for protein expression data
#'
#' This data consists of two batches and two conditions
#' corresponding to case and control for the protein expression data
#'
#' @name protein_example_data
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'     \item{Arrayname}{Array Name}
#'     \item{samplename}{Sample Name}
#'     \item{Batch}{Batch Indicator}
#'     \item{category}{Condition (Case vs Control) Indicator}
#' }
#' @return Protein data sample info
"protein_sample_info"

#' Protein data with 39 protein expression levels
#'
#' This data consists of two batches and two conditions
#' corresponding to case and control
#'
#' @name protein_example_data
#' @format A data frame with 39 rows and 24 variables:
#' \describe{
#'     \item{Columns1-24}{Control and Case samples}
#'     \item{rows1-39}{Proteins 1-39}
#' }
#' @return Protein data
"protein_data"
