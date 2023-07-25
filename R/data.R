globalVariables(c("protein_sample_info", "protein_data", "batch_indicator",
    "signature_data", "bladder_meta", "bladder_data"))

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
#' This dataset is from signature data captured when activating different growth
#' pathway genes in human mammary epithelial cells (GEO accession: GSE73628).
#' This data consists of three batches and ten different conditions
#' corresponding to control and nine different pathways.
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

#' Real bladderbatch dataset
#'
#' This dataset is from bladder cancer data. This dataset has 57 bladder samples
#' with 5 batches and 3 covariate levels (cancer, biopsy, control).
#' Batch 1 contains only cancer, 2 has cancer and controls, 3 has only controls,
#' 4 contains only biopsy, and 5 contains cancer and biopsy
#'
#' @name bladder_meta
#' @docType data
#' @format A data frame with 57 rows and 3 variables:
#' \describe{
#'     \item{samples}{sample ID}
#'     \item{batch}{batch}
#'     \item{condition}{condition}
#' }
#' @keywords datasets
#' @usage data(bladder_meta)
"bladder_meta"

#' Bladder cancer data with 22,283 microarray gene expression data
#'
#' This data consists of five batches and three conditions.
#' The columns are samples, and the rows represent
#' 22,283 different microarray gene expression data.
#'
#' @name bladder_data
#' @docType data
#' @format A data frame with 22,283 rows and 57 variables
#' @keywords datasets
#' @usage data(bladder_data)
"bladder_data"

