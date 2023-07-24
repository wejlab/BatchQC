globalVariables(c("protein_sample_info", "protein_data", "batch_indicator", "signature_data"))

#' Batch and Condition indicator for protein expression data
#'
#' This data consists of two batches and two conditions
#' corresponding to case and control for the protein expression data
#'
#' @name protein_sample_info
#' @docType data
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'     \item{Arrayname}{Array Name}
#'     \item{samplename}{Sample Name}
#'     \item{Batch}{Batch Indicator}
#'     \item{category}{Condition (Case vs Control) Indicator}
#' }
#' @keywords datasets
#' @usage data(protein_sample_info)
"protein_sample_info"

#' Protein data with 39 protein expression levels
#'
#' This data consists of two batches and two conditions
#' corresponding to case and control. The columns are case/control
#' samples, and the rows represent 39 different proteins.
#'
#' @name protein_data
#' @docType data
#' @format A data frame with 39 rows and 24 variables
#' @keywords datasets
#' @usage data(protein_data)
"protein_data"


#' Batch and Condition indicator for signature data
#'
#' This data consists of three batches and ten conditions
#' corresponding to case and control for the signature data
#'
#' @name batch_indicator
#' @docType data
#' @format A data frame with 89 rows and 3 variables:
#' \describe{
#'     \item{samples}{sample ID}
#'     \item{batch}{batch}
#'     \item{condition}{condition}
#' }
#' @keywords datasets
#' @usage data(batch_indicator)
"batch_indicator"

#' Signature data with 1600 gene expression levels
#'
#' This data consists of three batches and ten conditions.
#' The columns are samples, and the rows represent
#' 1600 different genes.
#'
#' @name signature_data
#' @docType data
#' @format A data frame with 1600 rows and 89 variables
#' @keywords datasets
#' @usage data(signature_data)
"signature_data"

