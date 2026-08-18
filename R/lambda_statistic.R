#' Compute the lambda index for determining a need for batch correction
#'
#' This function calculates the proportions of variation explained by batch,
#' group, and residual for each gene using two-way ANOVA and computes the lambda
#' index based on these three proportions.
#'
#' @param dat Numeric matrix of dimension (genes x samples) where each row
#'   represents one gene's expression across samples.
#' @param batchind Factor or numeric vector of length = ncol(dat); batch
#'   indicator for each sample
#' @param groupind Factor or numeric vector of length = ncol(dat); biological
#'   group label/indicator for each sample.
#' @importFrom stats lm anova
#' @return dataframe with columns:
#'   \describe{
#'     \item{BatchV}{Proportion of total variance explained by batch effects.}
#'     \item{GroupV}{Proportion of total variance explained by group effects.}
#'     \item{ResidV}{Proportion of total variance that is residual noise.}
#'     \item{lambda_raw}{Raw lambda index = total SS_batch / total SS_group.}
#'     \item{lambda_adj}{Adjusted lambda = lambda_raw * ResidV/(1-ResidV).}
#'   }
#' @examples
#'
#' library(scran)
#' se <- mockSCE()
#' res <- BatchQC::compute_lambda(assays(se)[["counts"]],
#'     colData(se)$Mutation_Status,
#'     colData(se)$Treatment)
#' print(res)
#'
#' @export

compute_lambda <- function(dat, batchind, groupind) {
    dat <- t(dat)
    n <- ncol(dat)
    expvar <- t(apply(dat, 2, function(x) {
        fit <- lm(x ~ factor(batchind) + factor(groupind))
        av  <- anova(fit)
        av$`Sum Sq`
    }))

    ss <- colSums(expvar)
    ss_list <- ss / sum(ss)
    lambda_raw <- ss[1] / ss[2]
    lambda_adj <- lambda_raw * ss_list[3] / (1 - ss_list[3])
    lambda_adj <- log(lambda_adj)

    res <- c(BatchV = ss_list[1], GroupV = ss_list[2], ResidV = ss_list[3],
        lambda_raw = lambda_raw, lambda_adj = lambda_adj)

    res <- t(data.frame(res))
    return(res)
}

#' Check if the experimental design is balanced or unbalanced
#'
#' Used in conjunction with the lambda
#'
#' @param se summarized experiment object
#' @param covariate string, biological covariate
#' @param batch string, batch variable
#' @return Boolean Value, TRUE if the experimental design is balanced, FALSE if
#'   the experimental design is not balanced
#' @examples
#'
#' library(scran)
#' se <- mockSCE()
#' balanced_design_check <- is_design_balanced(se, batch = "Mutation_Status",
#'                                                 covariate = "Treatment")
#' balanced_design_check
#'
#' @export

is_design_balanced <- function(se, batch, covariate) {
    balanced <- FALSE
    b_design <- batch_design(se, batch, covariate)
    pearson <- std_pearson_corr_coef(b_design)
    cramers <- cramers_v(b_design)

    if (pearson == 0 & cramers == 0) {
        balanced <- TRUE
    }

    return(balanced)
}

#' Provide a recommendation on batch correction based on lambda calculation
#'
#' This functions determines if an experimental design is balanced, then
#' calculates the lambda statistic for balanced designs and provides a
#' recommendation on if batch correction should be utilized. In general,
#' unbalanced designs always benefit from batch correction, while balanced
#' designs with a lambda greater than -7 benefit from batch correction.
#'
#' @param se summarized experiment object
#' @param assay string, the assay to analyze
#' @param condition string, condition variable
#' @param batch string, batch variable
#' @return a named list with:
#'   \describe{
#'     \item{lambda_stat}{provides the output of `compute_lambda` function}
#'     \item{correction_recommendation}{string, rec for batch correction}
#'     }
#' @examples
#'
#' library(scran)
#' se <- mockSCE()
#' lambda_calculation <- run_lambda(
#'                             se,
#'                             assay = "counts",
#'                             batch = "Mutation_Status",
#'                             condition = "Treatment")
#' print(lambda_calculation$correction_recommendation)
#' print(lambda_calculation$lambda_stat)
#'
#' @return a list with 2 parameters, 'lambda_stat' which contains the adj lambda
#'   value from lambda_compute (ln(lambda)) and 'correction_recommendation'
#'   which contains a string with a recommendation on if batch correction
#'   should be completed
#' @export

run_lambda <- function(se, assay, batch, condition) {
    recommendation <- NULL
    lambda_res <- NULL
    LAMBDA_THRESHOLD <- -7

    lambda_res <- data.frame(compute_lambda(assays(se)[[assay]],
        colData(se)[, batch],
        colData(se)[, condition]))

    if (is_design_balanced(se, batch, condition)) {
        if (lambda_res$lambda_adj > LAMBDA_THRESHOLD) {
            recommendation <- paste0("The experimental design is balanced and ",
                "the lambda statistic is ",
                round(lambda_res$lambda_adj, digits = 2),
                ", which is greater than -7. Therefore, you should consider ",
                "batch correction.")
        }else {
            recommendation <- paste0("The experimental design is balanced and ",
                "the lambda statistic is ",
                round(lambda_res$lambda_adj, digits = 2),
                ", which is less than or equal to -7. Therefore, you should ",
                "consider using your data WITHOUT batch correction.")
        }
    }else {
        recommendation <- paste0("The experimental design is unbalanced. ",
            "Therefore, you should consider applying a batch correction ",
            "method to your data. The calculated lambda statistic is ",
            round(lambda_res$lambda_adj, digits = 2), ".")
    }

    return(list(lambda_stat = lambda_res$lambda_adj,
        correction_recommendation = recommendation))
}
