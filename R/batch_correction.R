#' Batch Correct
#' This function allows you to Add batch corrected count matrix to the SE object
#' @param se SummarizedExperiment object
#' @param method Normalization Method
#' @param assay_to_normalize Which assay use to do normalization
#' @param batch The batch
#' @param group The group variable
#' @param covar Covariate Matrix
#' @param output_assay_name name of results assay
#' @usage batch_correct(se, method, assay_to_normalize, batch, group = NULL,
#' covar, output_assay_name)
#' @return a summarized experiment object with normalized assay appended
#' @import SummarizedExperiment
#' @import sva
#'
#' @export
batch_correct <- function(se, method, assay_to_normalize, batch, group = NULL,
    covar, output_assay_name) {
    se <- se
    batch <- data.frame(colData(se))[, batch]
    if (method == 'ComBat-Seq') {
        se <- combat_seq_correction(se, assay_to_normalize, batch, group, covar,
            output_assay_name)
    } else if (method == 'ComBat') {
        se <- combat_correction(se, assay_to_normalize, batch, covar,
            output_assay_name)
    }
    return(se)
}

#' Combat-Seq Correction
#' This function applies combat-seq correction to your summarized experiment
#' object
#' @param se SummarizedExperiment object
#' @param assay_to_normalize Assay that should be corrected
#' @param batch The variable that represents batch
#' @param group The group variable
#' @param covar Covariate Matrix
#' @param output_assay_name name of results assay
#' @usage combat_seq_correction(se, assay_to_normalize, batch, group, covar,
#' output_assay_name)
#' @return SE object with an added combat-seq corrected array
#' @import SummarizedExperiment
#' @import sva

combat_seq_correction <- function(se, assay_to_normalize, batch,
    group, covar, output_assay_name) {
    if (is.null(covar)) {
        assays(se)[[output_assay_name]] <- ComBat_seq(as.matrix(
            assays(se)[[assay_to_normalize]]), batch = batch)
    } else {
        if (length(covar) == 1) {
            cov <- data.frame(colData(se))[, covar]
            cov <- as.factor(cov)
            cov <- as.numeric(cov)
            cov <- as.matrix(cov)
            rownames(cov) <- rownames(data.frame(colData(se)))

            if (!is.null(group)) {
                assays(se)[[output_assay_name]] <- ComBat_seq(
                    as.matrix(assays(se)[[assay_to_normalize]]),
                    batch = batch, covar_mod = cov, group = group,
                    full_mod = TRUE)
            } else {
                assays(se)[[output_assay_name]] <- ComBat_seq(as.matrix(
                    assays(se)[[assay_to_normalize]]),
                    batch = batch, covar_mod = cov, group = group)
            }
        } else {
            cov <- data.frame(colData(se))[, covar]
            for (i in seq_len(ncol(cov))) {
                cov[, i] <- as.factor(cov[, i])
                cov[, i] <- as.numeric(cov[, i])
            }

            if (!is.null(group)) {
                assays(se)[[output_assay_name]] <- ComBat_seq(as.matrix(
                    assays(se)[[assay_to_normalize]]),
                    batch = batch, covar_mod = cov, group = group,
                    full_mod = TRUE)
            } else {
                assays(se)[[output_assay_name]] <- ComBat_seq(as.matrix(
                    assays(se)[[assay_to_normalize]]),
                    batch = batch, covar_mod = cov, group = group)
            }
        }
    }
    return(se)
}

#' Combat Correction
#' This function applies combat correction to your summarized experiment object
#' @param se SummarizedExperiment object
#' @param assay_to_normalize Assay that should be corrected
#' @param batch The variable that represents batch
#' @param covar Covariate Matrix
#' @param output_assay_name name of results assay
#' @return SE object with an added combat corrected array
#' @import SummarizedExperiment
#' @import sva

combat_correction <- function(se, assay_to_normalize, batch,
    covar, output_assay_name) {
    if (is.null(covar)) {
        assays(se)[[output_assay_name]] <-
            ComBat(dat = assays(se)[[assay_to_normalize]], batch = batch)
    } else {
        if (length(covar) == 1) {
            cov <- data.frame(colData(se))[, covar]
            cov <- as.factor(cov)
            cov <- as.numeric(cov)
            cov <- data.frame(cov)
            colnames(cov) <- covar
            rownames(cov) <- rownames(data.frame(colData(se)))

            model <- stats::model.matrix(stats::as.formula(
                paste0('~', colnames(cov))), data = cov)
            results <- ComBat(dat = assays(se)[[assay_to_normalize]],
                batch = batch,
                mod = model)
            results[is.na(results)] <- 0
            assays(se)[[output_assay_name]] <- results
        } else {
            cov <- data.frame(colData(se))[, covar]

            for (i in seq_len(ncol(cov))) {
                cov[, i] <- as.factor(cov[, i])
                cov[, i] <- as.numeric(cov[, i])
            }

            cov <- data.frame(cov)
            rownames(cov) <- rownames(data.frame(colData(se)))
            colnames(cov) <- covar

            linearmodel <- stats::as.formula(paste0('~',
                paste(colnames(cov),
                    sep = '+')))
            model <- stats::model.matrix(linearmodel, data = cov)

            results <- ComBat(dat = assays(se)[[assay_to_normalize]],
                batch = batch,
                mod = model)
            results[is.na(results)] <- 0
            assays(se)[[output_assay_name]] <- results

        }
    }
    return(se)
}
