cor_props <- function(bd){
  #' Calculate correlation properties on a batch_design matrix `bd`

  # Subset matrix to design only
  m = bd[, -1, ]
  rowsums = rowSums(m)
  colsums = colSums(m)
  tablesum = sum(rowsums)
  expected = matrix(0, nrow(m), ncol(m))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      expected[i, j] = rowsums[i] * colsums[j]/tablesum
    }
  }
  chi = sum((m - expected)^2/expected)
  mmin = min(nrow(m), ncol(m))

  out = list("chi" = chi, "mmin"=mmin, "tablesum"=tablesum)
  return(out)
}

std_pearson_corr_coef <- function(bd) {
  #' Calculate standardized Pearson correlation coefficient
  c <- cor_props(bd)
  r <- sqrt(c$chi * c$mmin/((c$chi + c$tablesum) * (c$mmin - 1)))
  return(r)
}

cramers_v <- function(bd) {
  # Calculate Cramer's V
  c <- cor_props(bd)
  v <- sqrt(c$chi/(c$tablesum * (c$mmin - 1)))
  return(v)
}

confound_metrics <- function(se){
  covs = metadata(se)$covariates
  metrics <- list("Pearson Correlation Coefficient"=std_pearson_corr_coef, "Cramer's V"=cramers_v)
  metric.mat <- matrix(nrow=length(covs), ncol=length(metrics), dimnames = list(covs, names(metrics)))

  for (c in covs){
    # Get batch design
    bd <- batch_design(se, c)
    for (m in names(metrics)){
      # Compute metric and place in appropriate slot
      metric.mat[c, m] <- metrics[[m]](bd)
    }
  }
  # Add metrics to se
  return(metric.mat)
}
