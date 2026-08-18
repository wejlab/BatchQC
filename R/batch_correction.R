globalVariables(c("mod"))
#' Batch Correct
#' This function allows you to Add batch corrected count matrix to the SE object
#' @param se SummarizedExperiment object
#' @param method Normalization Method ("ComBat-Seq", "ComBat", "limma", "sva",
#' svaseq, "Harman")
#' @param assay_to_normalize Which assay use to do normalization
#' @param batch The batch
#' @param group The group variable
#' @param covar list of covariates
#' @param output_assay_name name of results assay
#' @param psva boolean; default: FALSE. Only used if normalization method is
#' "sva". If set to TRUE and no covariate input, psva function from the sva
#' package will be used to remove batch effect.
#' @param num_sv boolean; default: FALSE. Only used if normalization method is
#' "svaseq". The number of estimated latent factor is set to 1 for a small
#' number of samples. If set to TRUE, svaseq function will estimate the number
#' of latent factors for you.
#' @param limit numeric; default: 0.95. Only used if normalization method is
#' "Harman". Indicates the limit of confidence in which to stop removing a batch
#' effect. Must be between 0 and 1.
#' @param numrepeats integer; default: 100000L. Only used if normalization
#' method is "Harman". The number of repeats in which to run the simulated batch
#' mean distribution estimator using the random selection algorithm.
#' @usage batch_correct(se, method, assay_to_normalize, batch, group = NULL,
#' covar, output_assay_name, psva, num_sv, limit, numrepeats)
#' @return a summarized experiment object with normalized assay appended
#' @import SummarizedExperiment
#' @import sva
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se <- BatchQC::batch_correct(se, method = "ComBat-Seq",
#'                                     assay_to_normalize = "counts",
#'                                     batch = "Mutation_Status",
#'                                     covar = "Treatment",
#'                                     output_assay_name =
#'                                         "ComBat_Seq_Corrected")
#' se <- BatchQC::batch_correct(se, method = "ComBat",
#'                                     assay_to_normalize = "counts",
#'                                     batch = "Mutation_Status",
#'                                     covar = "Treatment",
#'                                     output_assay_name =
#'                                         "ComBat_Corrected")
#' se
#'
#' @export
batch_correct <- function(se, method, assay_to_normalize, batch, group = NULL,
    covar, output_assay_name, psva = FALSE, num_sv = FALSE, limit = 0.95,
    numrepeats = 100000L) {
    se <- se
    var_of_interest <- batch
    batch <- data.frame(colData(se))[, batch]
    if (method == 'ComBat-Seq') {
        se <- ComBat_seq_correction(se, assay_to_normalize, batch, group, covar,
            output_assay_name)
    } else if (method == 'ComBat') {
        se <- ComBat_correction(se, assay_to_normalize, batch, covar,
            output_assay_name)
    } else if (method == 'limma') {
        se <- limma_correction(se, assay_to_normalize, batch, covar,
            output_assay_name)
    } else if (method == 'sva') {
        se <- sva_correction(se, assay_to_normalize, var_of_interest, covar,
            output_assay_name, psva)
    } else if (method == "svaseq") {
        se <- svaseq_correction(se, assay_to_normalize, var_of_interest, covar,
            output_assay_name, num_sv)
    } else if (method == "Harman") {
        se <- Harman_correction(se, assay_to_normalize, batch, covar,
            output_assay_name, limit, numrepeats)
    }
    return(se)
}

#' ComBat-Seq Correction
#' This function applies ComBat-seq correction to your summarized experiment
#' object
#' @param se SummarizedExperiment object
#' @param assay_to_normalize Assay that should be corrected
#' @param batch The variable that represents batch
#' @param group The group variable
#' @param covar list of covariates
#' @param output_assay_name name of results assay
#' @usage ComBat_seq_correction(se, assay_to_normalize, batch, group, covar,
#' output_assay_name)
#' @return SE object with an added ComBat-seq corrected array
#' @import SummarizedExperiment
#' @import sva

ComBat_seq_correction <- function(se, assay_to_normalize, batch,
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

#' ComBat Correction
#' This function applies ComBat correction to your summarized experiment object
#' @param se SummarizedExperiment object
#' @param assay_to_normalize Assay that should be corrected
#' @param batch The variable that represents batch
#' @param covar list of covariates
#' @param output_assay_name name of results assay
#' @return SE object with an added ComBat corrected array
#' @import SummarizedExperiment
#' @import sva

ComBat_correction <- function(se, assay_to_normalize, batch,
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

#' Limma Correction
#' This function applies limma batch correction to your provided assay
#' @param se SummarizedExperiment object
#' @param assay_to_normalize Log assay that should be corrected
#' @param batch Factor containing batch information
#' @param covar list of covariates
#' @param output_assay_name name of results assay
#' @return SE object with an added limma corrected array
#' @import SummarizedExperiment
#' @importFrom limma removeBatchEffect
#'
limma_correction <- function(se, assay_to_normalize, batch, covar,
    output_assay_name) {
    if (is.null(covar)) {
        limma_corrected <- limma::removeBatchEffect(
            assays(se)[[assay_to_normalize]],
            batch = batch
            #design = model.matrix(~)
        )
    }else if (length(covar) == 1) {
        cov <- as.numeric(colData(se)[[covar]])
        limma_corrected <- limma::removeBatchEffect(
            assays(se)[[assay_to_normalize]],
            batch = batch,
            covariates = cov
        )
    }else {
        cov <- data.frame(colData(se))[, covar]

        for (i in seq_len(ncol(cov))) {
            cov[, i] <- as.factor(cov[, i])
            cov[, i] <- as.numeric(cov[, i])
        }

        cov <- data.frame(cov)
        rownames(cov) <- rownames(data.frame(colData(se)))
        colnames(cov) <- covar

        limma_corrected <- limma::removeBatchEffect(
            assays(se)[[assay_to_normalize]],
            batch = batch,
            covariates = cov
        )
    }
    assays(se)[[output_assay_name]] <- limma_corrected
    return(se)
}

#' sva Correction
#' This function applies sva correction to a summarized experiment object
#' (implementation adapted from sva::psva)
#' @param se SummarizedExperiment object
#' @param assay_to_normalize string; name of assay that should be corrected
#' @param var_of_interest string; name of  experimental variable of interest
#' @param covar list; sting list  of covariates to include in sva analysis
#' @param output_assay_name string; name of results assay
#' @param psva boolean; default: FALSE. If set to TRUE and no covariate input,
#' psva function from the sva package will be used to remove batch effect.
#' @return SE object with an added sva corrected array
#' @usage sva_correction(
#'     se,
#'     assay_to_normalize,
#'     var_of_interest,
#'     covar,
#'     output_assay_name,
#'     psva = FALSE)
#' @import SummarizedExperiment
#' @import sva

sva_correction <- function(se, assay_to_normalize, var_of_interest,
    covar, output_assay_name, psva = FALSE) {
    if (is.null(covar)) {
        mod0 <- model.matrix(~1, data = colData(se))
        mod1_formula <- as.formula(paste0("~", var_of_interest))
        mod1 <- model.matrix(mod1_formula, data = colData(se))
        n.sv <- sva::num.sv(assays(se)[[assay_to_normalize]], mod1,
            method = "leek")
        if (psva) {
            sva_assay <- sva::psva(
                dat = assays(se)[[assay_to_normalize]],
                batch = data.frame(colData(se))[, var_of_interest],
                mod = mod,
                mod0 = mod0,
                n.sv = n.sv)
        }
    }else {
        if (length(covar) == 1) {
            mod1_formula <- as.formula(paste0("~", var_of_interest, "+", covar))
            mod0_formula <- as.formula(paste0("~", covar))
        } else {
            mod1_formula <- as.formula(paste0("~", var_of_interest, "+",
                                            paste(covar, collapse = "+")))
            mod0_formula <- as.formula(paste0("~",
                                            paste(covar, collapse = "+")))
        }
        mod1 <- model.matrix(mod1_formula, data = colData(se))
        mod0 <- model.matrix(mod0_formula, data = colData(se))
    }
    sva_object <- sva::sva(
        dat = assays(se)[[assay_to_normalize]],
        mod = mod1,
        mod0 = mod0
    )

    fsva_object <- sva::fsva(
        dbdat = assays(se)[[assay_to_normalize]],
        mod = mod0,
        sv = sva_object,
        newdat = assays(se)[[assay_to_normalize]],
        method = "fast"
        )
    fsva_adjust_db <- fsva_object$db
    colnames(fsva_adjust_db) <- colnames(assays(se)[[assay_to_normalize]])
    assays(se)[[output_assay_name]] <- fsva_adjust_db
    return(se)
}

#' svaseq Correction
#' This function applies sva correction to a summarized experiment object
#' with count based RNA-seq data
#' @param se SummarizedExperiment object
#' @param assay_to_normalize string; name of assay that should be corrected
#' @param var_of_interest string; name of  experimental variable of interest
#' @param covar list; sting list  of covariates to include in sva analysis
#' @param output_assay_name string; name of results assay
#' @param num_sv boolean; Default is FALSE: the number of estimated latent
#' factor is set to 1 for a small number of samples. If set to TRUE, svaseq
#' function will estimate the number of latent factors for you.
#' @return SE object with an added sva corrected array
#' @usage svaseq_correction(
#'     se,
#'     assay_to_normalize,
#'     var_of_interest,
#'     covar,
#'     output_assay_name,
#'     num_sv = FALSE)
#' @import SummarizedExperiment
#' @import sva

svaseq_correction <- function(se, assay_to_normalize, var_of_interest,
    covar, output_assay_name, num_sv = FALSE) {
    dat <- assays(se)[[assay_to_normalize]]
    if (is.null(covar)) {
        mod0 <- model.matrix(~1, data = colData(se))
        mod1_formula <- as.formula(paste0("~", var_of_interest))
        mod1 <- model.matrix(mod1_formula, data = colData(se))
    }else {
        if (length(covar) == 1) {
            mod1_formula <- as.formula(paste0("~", var_of_interest, "+", covar))
            mod0_formula <- as.formula(paste0("~", covar))
        } else {
            mod1_formula <- as.formula(paste0("~", var_of_interest, "+",
                                            paste(covar, collapse = "+")))
            mod0_formula <- as.formula(paste0("~",
                                            paste(covar, collapse = "+")))
        }
        mod1 <- model.matrix(mod1_formula, data = colData(se))
        mod0 <- model.matrix(mod0_formula, data = colData(se))
    }
    if (num_sv) {
        batch_unsup_sva <- svaseq(dat, mod1, mod0)$sv
    } else {
        batch_unsup_sva <- svaseq(dat, mod1, mod0, n.sv = 1)$sv
    }
    colnames(batch_unsup_sva) <- paste('sv', seq_len(ncol(batch_unsup_sva)))
    mod1Sv <- cbind(mod1, batch_unsup_sva)
    psva.fit <- lmFit(dat, mod1Sv)
    sv_coef <- psva.fit$coefficients[, colnames(batch_unsup_sva), drop = FALSE]
    sv_effects <- sv_coef %*% t(batch_unsup_sva)
    svaseq_assay <- dat - sv_effects
    assays(se)[[output_assay_name]] <- svaseq_assay
    return(se)
}

#' Harman Correction
#' This function applies Harman correction to a summarized experiment object
#' with and reconstructs the data back into the original feature space
#' @param se SummarizedExperiment object
#' @param assay_to_normalize string; name of assay that should be corrected
#' @param batch factor; The variable that represents batch
#' @param covar string; name of covariate.
#' @param output_assay_name string; name of results assay
#' @param limit numeric; default: 0.95 Indicates the limit of confidence in
#' which to stop removing a batch effect. Must be between 0 and 1
#' @param numrepeats integer; default: 100000L the number of repeats in which
#' to run the simulated batch mean distribution estimator using the random
#' selection algorithm.
#' @usage Harman_correction(
#'     se,
#'     assay_to_normalize,
#'     batch,
#'     covar,
#'     output_assay_name,
#'     limit = 0.95,
#'     numrepeats = 100000L)
#' @return SE object with an added Harman corrected reconstructed data
#' @import SummarizedExperiment
#' @import Harman

Harman_correction <- function(se, assay_to_normalize, batch,
                                covar, output_assay_name, limit = 0.95,
                                numrepeats = 100000L) {
    if (!is.factor(batch)) {
        batch <- as.factor(batch)
    }
    dat <- assays(se)[[assay_to_normalize]]
    if (is.null(covar)) {
        cov <- NULL
    }else if (length(covar) > 1) {
        warning("Harman correction only supports NULL covar or one covar.
                Setting covar = NULL")
        cov <- NULL
    }else {
        cov <- as.factor(colData(se)[[covar]])
    }

    Harman_res <- harman(datamatrix = dat, expt = cov, batch = batch,
                        limit = limit, numrepeats = numrepeats)
    assays(se)[[output_assay_name]] <- as.data.frame(reconstructData(
        Harman_res))
    return(se)
}
