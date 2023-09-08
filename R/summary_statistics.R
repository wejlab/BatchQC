#' This function allows you to make a batch design matrix
#' @param se summarized experiment
#' @param covariate biological covariate
#' @param batch batch variable
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#' @return design table
#' @example R/examples/batch_design.R
#' @export

batch_design <- function(se, batch, covariate) {
    ### Create a batch design table for the provided covariate
    design <- colData(se) %>% as_tibble %>%
        group_by_at(covariate) %>%
        dplyr::count(across(batch)) %>%
        pivot_wider(names_from = batch, values_from = n)
    design <- replace(design, is.na(design), 0)
    return(design)
}


#' This function allows you to calculate correlation properties
#' @param bd batch design
#' @return correlation properties
#' @example R/examples/cor_props.R
#' @export

cor_props <- function(bd) {
    # Subset matrix to design only
    m <- bd[, -1, ]
    rowsums <- rowSums(m)
    colsums <- colSums(m)
    tablesum <- sum(rowsums)
    expected <- matrix(0, nrow(m), ncol(m))
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            expected[i, j] <- rowsums[i] * colsums[j] / tablesum
        }
    }
    chi <- sum((m - expected)^2 / expected)
    mmin <- min(nrow(m), ncol(m))

    out <- list("chi" = chi, "mmin" = mmin, "tablesum" = tablesum)
    return(out)
}


#' Calculate a standardized Pearson correlation coefficient
#' @param bd batch design
#' @return standardized Pearson correlation coefficient
#' @example R/examples/std_pearson_corr_coef.R
#' @export

std_pearson_corr_coef <- function(bd) {
    c <- cor_props(bd)
    r <- sqrt(c$chi * c$mmin / ((c$chi + c$tablesum) * (c$mmin - 1)))
    return(r)
}

#' This function allows you to calculate Cramer's V
#' @param bd batch design
#' @return Cramer's V
#' @example R/examples/cramers_v.R
#' @export
#'
cramers_v <- function(bd) {
    c <- cor_props(bd)
    v <- sqrt(c$chi / (c$tablesum * (c$mmin - 1)))
    return(v)
}

#'
#' Combine std. Pearson correlation coefficient and Cramer's V
#' @param se summarized experiment
#' @param batch batch variable
#' @return metrics of confounding
#' @example R/examples/confound_metrics.R
#' @export

confound_metrics <- function(se, batch) {
    # Covariates are non-batch
    cols <- names(colData(se))
    covs <- cols[cols != batch]
    metrics <- list("Pearson Correlation Coefficient" = std_pearson_corr_coef,
                    "Cramer's V" = cramers_v)
    metric.mat <- matrix(nrow = length(covs), ncol = length(metrics),
                            dimnames = list(covs, names(metrics)))

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
