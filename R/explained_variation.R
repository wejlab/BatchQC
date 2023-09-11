#' This function allows you to plot explained variation
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @import reshape2
#' @import ggplot2
#' @return List of explained variation by batch and condition
#' @examples
#' library(scran)
#' se <- mockSCE()
#' EV_boxplot <- BatchQC::EV_plotter(se, batch = "Mutation_Status",
#'                             condition = "Treatment", assay_name = "counts")
#' EV_boxplot
#'
#' @export
EV_plotter <- function(se, batch, condition, assay_name) {
    batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
    EV_boxplot <- ggplot(data =
            melt(as.data.frame(batchqc_ev$explained_variation),
                id.vars = NULL),
        aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "Percent Explained Variation") +
        labs(title = "Percent of Variation Explained by Source") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(list(EV_boxplot = EV_boxplot))
}

#' EV Table
#' Returns table with percent variation explained for specified number of genes
#'
#' @param se Summarized experiment object
#' @param batch Batch Covariates
#' @param condition  Condition covariate(s) of interest
#' @param assay_name Name of chosen assay
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#' @examples
#' library(scran)
#' se <- mockSCE()
#' EV_table <- BatchQC::EV_table(se, batch = "Mutation_Status",
#'                                     condition = "Treatment",
#'                                     assay_name = "counts")
#' EV_table
#'
#' @export
EV_table <- function(se, batch, condition, assay_name) {
    batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
    EV_table <- data.table(batchqc_ev$explained_variation[],
        keep.rownames = TRUE)
    colnames(EV_table)[1] <- "Gene Name"
    return(list(EV_table = EV_table))
}

#' Returns a list of explained variation by batch and condition
#' combinations
#'
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate(s) of interest
#' @param assay_name Assay of choice
#' @import rlist
#' @return List of explained variation by batch and condition
#' @examples
#' library(scran)
#' se <- mockSCE()
#' batchqc_explained_variation <- BatchQC::batchqc_explained_variation(se,
#'                                         batch = "Mutation_Status",
#'                                         condition = "Treatment",
#'                                         assay_name = "counts")
#' batchqc_explained_variation
#'
#' @export
batchqc_explained_variation <- function(se, batch, condition, assay_name) {
    df <- se@colData

    nlb <- n_distinct(as.data.frame(df[batch]))
    if (nlb <= 1) {
        batch_mod <- matrix(rep(1, ncol(se)), ncol = 1)
    } else {
        batch_mod <- stats::model.matrix(~df[[batch]])
    }

    nlc <- rep(0, length(condition))
    cond_mod <- list()
    mod <- list()
    cond_test <- list()
    cond_r2 <- list()
    cond_ps <- list()

    for (i in seq_len(length(condition))) {
        nlc[i] <- n_distinct(as.data.frame(df[condition[i]]))

        if (nlc[i] <= 1) {
            cond_mod[[i]] <- matrix(rep(1, ncol(se)), ncol = 1)
        } else {
            cond_mod[[i]] <- stats::model.matrix(~df[[condition[i]]])
        }

        mod[[i]] <- cbind(cond_mod[[i]], batch_mod[, -1])

        if (qr(mod[[i]])$rank < ncol(mod[[i]])) {
            options(error = NULL)
            if (ncol(mod[[i]]) == (nlb + 1)) {
                stop("A covariate is confounded with batch!
                        Please choose different covariates.")
            }
            if (ncol(mod[[i]]) > (nlb + 1)) {
                if ((qr(mod[[i]][, -c(seq_len(nlb))])$rank <
                        ncol(mod[[i]][, -c(seq_len(nlb))]))) {
                    stop('A covariate is confounded with batch!
                            Please choose different covariates.')
                }else {
                    stop("A covariate is confounded with batch!
                            Please choose different covariates.")
                }
            }
        }
    }

    mod2 <- list.cbind(cond_mod)
    mod2 <- cbind(mod2, batch_mod[, -1])
    cond_mod2 <- list.cbind(cond_mod)
    if (length(condition) > 1) {
        idx <- which(duplicated(colnames(mod2)) &
                colnames(mod2) == "(Intercept)")
        idx2 <- which(duplicated(colnames(cond_mod2)) &
                colnames(cond_mod2) == "(Intercept)")
        mod2 <- mod2[, -idx]
        cond_mod2 <- cond_mod2[, -idx2]
    }

    if (qr(mod2)$rank < ncol(mod2)) {
        options(error = NULL)
        if (ncol(mod2) == (nlb + 1)) {
            stop("A covariate is confounded with batch!
                    Please choose different covariates.")
        }
        if (ncol(mod2) > (nlb + 1)) {
            if ((qr(mod2[, -c(seq_len(nlb))])$rank <
                    ncol(mod2[, -c(seq_len(nlb))]))) {
                stop('At least one covariate is confounded with another!
                        Please choose different covariates.')
            }else {
                stop("At least one covariate is confounded with another!
                        Please choose different covariates.")
            }
        }
    }

    for (i in seq_len(length(condition))) {
        batch_mod2 <- matrix(nrow = nrow(mod2), ncol = 1)
        if (length(condition) > 1) {
            for (j in seq_len(length(condition))) {
                if (i == j) next
                batch_mod2 <- cbind(batch_mod2, cond_mod[[j]])
            }
        }

        batch_mod2 <- cbind(batch_mod2, batch_mod)
        idx <- which(duplicated(colnames(batch_mod2)) &
                colnames(batch_mod2) == "(Intercept)")
        if (length(idx) > 0) {
            batch_mod2 <- batch_mod2[, -idx]
        }

        batch_mod2 <- batch_mod2[, -1]

        cond_test[[i]] <- batchqc_f.pvalue(se, mod2, batch_mod2, assay_name)

        cond_ps[[i]] <- cond_test[[i]]$p
        cond_r2[[i]] <- cond_test[[i]]$r2_reduced
    }

    all_test <- batchqc_f.pvalue(se, mod2, cond_mod2, assay_name)

    batch_ps <- all_test$p

    r2_full <- all_test$r2_full
    batch_r2 <- all_test$r2_reduced

    explained_variation <- round(cbind(r2_full,
        batch_r2,
        list.cbind(cond_r2)), 5) * 100

    # Name columns according to batch and covariate names
    if (length(condition) == 1) {
        colnames(explained_variation)[1] <- "Full (Covariate + Batch)"
    } else {
        colnames(explained_variation)[1] <- "Full (Covariates + Batch)"
    }

    colnames(explained_variation)[2] <- "Batch"

    for (i in seq_len(length(condition))) {
        colnames(explained_variation)[i + 2] <- condition[i]
    }

    batchqc_ev <- list(explained_variation = explained_variation,
        cond_test = cond_test, batch_ps = batch_ps)

    return(batchqc_ev)
}
