globalVariables(c(glm.nb, AIC, glm, gaussian, glm.control))
#' Compute the AIC for lognormal (ComBat) model, negative binomial
#' (ComBat-seq) model and the Voom model
#'
#' This function calculates the AIC based on lognormal distribution,
#' negative binomial distribution as well as the voom transformation.
#' It then compares the AICs of the three models across different genes and
#' yields AIC-based metric values. Must have non-negative data and non-discrete
#' data will only analyze the lognormal and voom distribution.
#'
#' @param se SummarizedExperiment object
#' @param assay_of_interest The assay name from se that you are interested in
#'   analyzing. This assay need to be a counts assay containing only
#'   non-negative integers.
#' @param batchind Factor or numeric vector of length = ncol(dat);
#'   batch indicator for each sample.
#' @param groupind Factor or numeric vector of length = ncol(dat);
#'   biological group label/indicator for each sample.
#' @param maxit Integer giving the maximal number of IWLS iterations. Default is
#'   25.
#' @param zero_filt_percent Numeric value between 0 and 100, the percentage of
#'   zeros allowed for each gene to be included in the AIC calculation. Genes
#'   with more than this percentage of zeros will be filtered out. Default is
#'   100.
#'
#' @usage compute_aic(
#'     se,
#'     assay_of_interest,
#'     batchind,
#'     groupind,
#'     maxit = 25,
#'     zero_filt_percent = 100)
#' @description
#'   \describe{
#'     \item{nb_result}{A vector contains the AIC based on negative binomial
#'     model for individual genes.}
#'     \item{lognormal_result}{A vector contains the AIC based on lognormal
#'     model for individual genes.}
#'     \item{voom_result}{A vector contains the AIC based on voom transformation
#'     for individual genes.}
#'     \item{total_AIC}{The sum of AICs across all genes for the models in
#'     comparison.}
#'     \item{min_AIC}{The number of minimum AIC across the models in
#'     comparison for individual genes.}
#'     \item{AIC_score}{The ratios between total_AIC and min_AIC for the
#'     models in comparison. The optimal model is the one has minimum value.}
#'   }
#'
#' @return A list with a  boxplot of the AIC_boxplot and a dataframe containing
#' the total_AIC values, the frequency of the min_AIC values, the AIC_score, and
#' the median_AIC value, for the following three elements:
#' \describe{
#'     \item{nb}{The metric value calculated based on the negative binomial
#'       model.}
#'     \item{lognormal}{The metric value calculated based on the lognormal
#'       model.}
#'     \item{voom}{The metric value calculated based on the voom-based model.}
#'   }
#'
#' @examples
#' library(scran)
#' se <- mockSCE()
#' compare_aic <- compute_aic(se, assay_of_interest = "counts",
#'                             batchind = "Cell_Cycle",
#'                             groupind = c("Treatment", "Mutation_Status"))
#' print(compare_aic)
#'
#' @importFrom limma voom
#' @importFrom stats AIC gaussian glm glm.control
#' @importFrom MASS glm.nb
#' @import SummarizedExperiment
#' @export

compute_aic <- function(se, assay_of_interest, batchind,
                        groupind, maxit = 25, zero_filt_percent = 100) {
    dat <- assays(se)[[assay_of_interest]]
    analysis_design <- as.data.frame(colData(se)[c(groupind, batchind)])
    design <- stats::model.matrix(stats::as.formula(paste(" ~",
                            paste(colnames(analysis_design), collapse = "+"))),
                            data = analysis_design)
    nb_test <- TRUE
    num_models <- 3

    if (any(dat < 0)) {
        stop("Counts must be non-negative values only.")
    } else if (!all(dat == floor(dat))) {
        nb_test <- FALSE
        num_models <- 2
    }

    dat <- dat[rowSums(dat) != 0, ]
    dat <- dat[rowSums(dat == 0) <= zero_filt_percent / 100 * ncol(dat), ]

    aic_matrix <- run_AIC_models(dat, design, nb_test, maxit)
    aic_median <- apply(aic_matrix, 2, median, na.rm = TRUE)
    total_AIC <- as.data.frame(t(colSums(aic_matrix, na.rm = TRUE)),
        colnames = "")

    min_model <- apply(aic_matrix, 1, function(x) {
        if (all(is.na(x))) return(NA)
        which.min(x)})

    min_AIC <- t(as.data.frame(table(factor(min_model,
        levels = seq_along(seq(1, num_models)))))[2])
    colnames(min_AIC) <- colnames(aic_matrix)

    AIC_boxplot <- AIC_boxplots(aic_matrix, num_models)

    AIC_score <- as.data.frame(total_AIC / min_AIC, row.names = "")
    AIC_table <- rbind(total_AIC, min_AIC, AIC_score, aic_median)
    rownames(AIC_table) <- c("Total AIC", "Frequency of Minimum AIC",
        "AIC Score", "Median AIC")

    return(list(AIC_table = AIC_table, AIC_boxplot = AIC_boxplot))
}
#' Helper function that contains the code to run the lognormal, voom, and
#' negative binomial AIC models for `compute_aic`
#'
#' @importFrom limma voom
#' @importFrom stats AIC gaussian glm glm.control
#' @importFrom MASS glm.nb
#' @import SummarizedExperiment
#'
#' @param dat dataframe of the data to analyze
#' @param design stats design model to be used in the analyses
#' @param nb_test boolean; should negative binomial run (must be discrete data)
#' @param maxit integer; the max number of IWLS iterations
#'
#' @return data frame; containing the AIC results of each method
#'

run_AIC_models <- function(dat, design, nb_test, maxit) {
    lognormal_result <- apply(dat, 1, function(x) {
        tryCatch({
            lognormal_model <- glm(log(x + 1e-100) ~ design, family = gaussian)
            lognormal_AIC <- AIC(lognormal_model)
            return(lognormal_AIC)
        }, error = function(e) {
            return(NA)
        })})

    voom_dat <- voom(dat, design = design)
    voom_dat <- voom_dat$E
    voom_result <- apply(voom_dat, 1, function(x) {
        tryCatch({
            voom_lm_model <- lm(x ~ design)
            voom_lm_AIC <- AIC(voom_lm_model)
            return(voom_lm_AIC)
        }, error = function(e) {
            return(NA)
        })})

    if (nb_test) {
        nb_result <- apply(dat, 1, function(x) {
            tryCatch({
                nb_model <- glm.nb(x ~ design,
                    control = glm.control(maxit = maxit))
                nb_AIC <- AIC(nb_model)
                return(nb_AIC)
            }, error = function(e) {
                return(NA)
            })})

        aic_matrix <- cbind(nb_result, lognormal_result, voom_result)
        colnames(aic_matrix) <- c("nb_AIC", "lognormal_AIC", "voom_AIC")

    } else {
        aic_matrix <- cbind(lognormal_result, voom_result)
        colnames(aic_matrix) <- c("lognormal_AIC", "voom_AIC")
    }

    return(aic_matrix)
}

#' Boxplots for the distribution of AIC for each method
#'
#' This function creates a boxplot of all the AIC values for each gene under
#' each tested distribution to aid in identifying outliers
#'
#' @import tidyr
#'
#' @param AIC_data dataframe with the data to be plotted
#' @param num_methods integer representing the number of distribution methods
#'
#' @return AIC_boxplot; a boxplot for each method showing distribution of data

AIC_boxplots <- function(AIC_data, num_methods) {
    method <- "method"
    AIC_data <- AIC_data %>%
        as.data.frame() %>%
        rownames_to_column(var = "feature") %>%
        pivot_longer(cols = 2:(num_methods + 1),
            names_to = method,
            values_to = "AIC")

    AIC_plot <- ggplot(AIC_data, aes(x = method, y = AIC, color = method)) +
        geom_boxplot() +
        geom_point(position = position_jitterdodge(jitter.width = 0.2,
                                                    dodge.width = 0.75),
            alpha = 0.5)
}

