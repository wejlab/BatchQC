#' This function allows you to make a batch design matrix
#' @param se summarized experiment
#' @param covariate biological covariate
#' @param batch batch variable
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#' @return design table
#'
#' @export
batch_design <- function(se, batch, covariate){
  ### Create a batch design table for the provided covariate
  design <- colData(se) %>% as_tibble %>%
    group_by_at(covariate) %>%
    dplyr::count(across(batch)) %>%
    pivot_wider(names_from = batch, values_from = n) %>%
    replace(is.na(.), 0)
  return(design)
}


#' This function allows you to calculate correlation properties
#' @param bd batch design
#' @return correlation properties
#'
#' @export
cor_props <- function(bd){
  #Calculate correlation properties on a batch_design matrix `bd`

  # Subset matrix to design only
  m <- bd[, -1, ]
  rowsums <- rowSums(m)
  colsums <- colSums(m)
  tablesum <- sum(rowsums)
  expected <- matrix(0, nrow(m), ncol(m))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      expected[i, j] <- rowsums[i] * colsums[j]/tablesum
    }
  }
  chi <- sum((m - expected)^2/expected)
  mmin <- min(nrow(m), ncol(m))

  out <- list("chi" = chi, "mmin"=mmin, "tablesum"=tablesum)
  return(out)
}


#' This function allows you to calculate a standardized pearson corr coef
#' @param bd batch design
#' @return standardized pearson correlation coefficient
#'
#' @export
std_pearson_corr_coef <- function(bd) {
  #Calculate standardized Pearson correlation coefficient
  c <- cor_props(bd)
  r <- sqrt(c$chi * c$mmin/((c$chi + c$tablesum) * (c$mmin - 1)))
  return(r)
}


#' This function allows you to calculate cramer's V
#' @param bd batch design
#' @return cramer's V
#'
#' @export
cramers_v <- function(bd) {
  # Calculate Cramer's V
  c <- cor_props(bd)
  v <- sqrt(c$chi/(c$tablesum * (c$mmin - 1)))
  return(v)
}


#' This function allows you to combine std. pearson corr coef and cramer's V
#' @param se summarized experiment
#' @param batch batch variable
#' @return metrics of confounding
#'
#' @export
confound_metrics <- function(se, batch){
  # Covariates are non-batch
  cols <- names(colData(se))
  covs <- cols[cols != batch]
  metrics <- list("Pearson Correlation Coefficient"=std_pearson_corr_coef, "Cramer's V"=cramers_v)
  metric.mat <- matrix(nrow=length(covs), ncol=length(metrics), dimnames = list(covs, names(metrics)))

  for (c in covs){
    # Get batch design
    bd <- batch_design(se, batch, c)
    for (m in names(metrics)){
      # Compute metric and place in appropriate slot
      metric.mat[c, m] <- metrics[[m]](bd)
    }
  }
  # Add metrics to se
  return(metric.mat)
}
