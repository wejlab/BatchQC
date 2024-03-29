#' This function allows you to make a batch design matrix
#' @param se summarized experiment
#' @param covariate biological covariate
#' @param batch batch variable
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#' @return design table
#' @examples
#' library(scran)
#' se <- mockSCE()
#' batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
#'                                                 covariate = "Treatment")
#' batch_design_tibble
#'
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
#' @examples
#' library(scran)
#' se <- mockSCE()
#' batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
#'                                                 covariate = "Treatment")
#' correlation_property <- BatchQC::cor_props(batch_design_tibble)
#' correlation_property
#'
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
#' @examples
#' library(scran)
#' se <- mockSCE()
#' batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
#'                                                 covariate = "Treatment")
#' pearson_cor_result <- BatchQC::std_pearson_corr_coef(batch_design_tibble)
#' pearson_cor_result
#'
#' @export

std_pearson_corr_coef <- function(bd) {
    c <- cor_props(bd)
    r <- sqrt(c$chi * c$mmin / ((c$chi + c$tablesum) * (c$mmin - 1)))
    return(r)
}

#' This function allows you to calculate Cramer's V
#' @param bd batch design
#' @return Cramer's V
#' @examples
#' library(scran)
#' se <- mockSCE()
#' batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
#'                                                 covariate = "Treatment")
#' cramers_v_result <- BatchQC::cramers_v(batch_design_tibble)
#' cramers_v_result
#'
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
#' @examples
#' library(scran)
#' se <- mockSCE()
#' confound_table <- BatchQC::confound_metrics(se, batch = "Mutation_Status")
#' confound_table
#'
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
