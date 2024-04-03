#' This function allows you to plot explained variation
#' @param batchqc_ev table of explained variation from batchqc_explained_variation
#' @import reshape2
#' @import ggplot2
#' @return boxplot of explained variation
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' se$Treatment <- as.factor(se$Treatment)
#' expl_var_result <- batchqc_explained_variation(se, batch = "Mutation_Status",
#'                             condition = "Treatment", assay_name = "counts")
#' EV_boxplot <- BatchQC::EV_plotter(expl_var_result[[1]])
#' EV_boxplot
#'
#' @export
EV_plotter <- function(batchqc_ev) {
    EV_boxplot <- ggplot(data =
            melt(as.data.frame(batchqc_ev),
                id.vars = NULL),
        aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        scale_x_discrete(name = "") +
        scale_y_continuous(name = "Percent Explained Variation") +
        labs(title = "Percent of Variation Explained by Source") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    return(EV_boxplot)
}

#' EV Table
#' Returns table with percent variation explained for specified number of genes
#'
#' @param batchqc_ev explained variation results from batchqc_explained_variation
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' se$Treatment <- as.factor(se$Treatment)
#' exp_var_result <- BatchQC::batchqc_explained_variation(se, batch = "Mutation_Status",
#'                                     condition = "Treatment",
#'                                     assay_name = "counts")
#' EV_table <- BatchQC::EV_table(exp_var_result[[1]])
#'
#' EV_table
#'
#' @export
EV_table <- function(batchqc_ev) {
    EV_table <- data.table(batchqc_ev,
        keep.rownames = TRUE)
    colnames(EV_table)[1] <- "Gene Name"
    return(EV_table = EV_table)
}

#' Returns a list of explained variation by batch and condition
#' combinations
#'
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate(s) of interest if desired, default is NULL
#' @param assay_name Assay of choice
#' @importFrom stats formula model.matrix
#' @import SummarizedExperiment
#' @return List of explained variation by batch and condition
#'
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
batchqc_explained_variation <- function(se, batch, condition = NULL, assay_name) {
    if (!is.factor(se[[batch]])) {
        #"The batch variable contianed in your se object must be a factor."
        return()
    }else if (!is.null(condition)) {
        for (variable in condition){
            if (!is.factor(se[[variable]])) {
                #"All condition variables in your se object must be a factor."
                return()
            }
        }
    }



    sample_info <- SummarizedExperiment::colData(se)
    assay_dat <- t(assay(se, assay_name))

    ### Check if batch and conditions are linearly independent
    ### Check if batch and condition variables are factors (in Shiny, only let the user choose batch and conditions that are factors)

    ## Total RSS
    mus <- matrix(colMeans(assay_dat),
        nrow(assay_dat),
        ncol(assay_dat),
        byrow = TRUE)
    res_total <- colSums((assay_dat - mus)^2)

    ## Full RSS and SST
    covs <- c(batch, condition)
    model_full <- stats::formula(paste("~", paste(covs, collapse = " + ")))
    design_full <- stats::model.matrix(model_full, data = sample_info)
    res_full <- get.res(assay_dat, design_full)
    EV_table_ind <- EV_table_type2 <- data.frame(Explained = res_total -
            res_full)

    ### individual SSE and Type 2 SSE
    for (j in seq_len(length(covs))){
        ### individual
        model_ind <- stats::formula(paste("~ ", covs[j], sep = ''))
        design_ind <- stats::model.matrix(model_ind, data = sample_info)
        res_ind <-  get.res(assay_dat, design_ind)
        EV_table_ind[covs[j]] <- res_total - res_ind

        ### Type2
        if (is.null(condition)) {
            EV_table_type2[covs[j]] <- EV_table_ind[covs[j]]
        } else {
            model_alt <- stats::formula(paste("~",
                paste(covs[-j], collapse = " + ")))
            design_alt <- stats::model.matrix(model_alt, data = sample_info)
            res_alt <- get.res(assay_dat, design_alt)
            EV_table_type2[covs[j]] <- res_alt - res_full
        }
    }

    EV_table_ind["Unexplained"] <- EV_table_type2["Unexplained"] <- res_full
    EV_table_ind <- round(100 * EV_table_ind / res_total, 2)
    EV_table_type2 <- round(100 * EV_table_type2 / res_total, 2)

    return(list(EV_table_ind = EV_table_ind, EV_table_type2 = EV_table_type2))
}

#' Helper function to get residuals
#'
#' @param y assay
#' @param X model matrix design
#' @return residuals
get.res <- function(y, X) {
    colSums((y - X %*% solve(t(X) %*% X) %*% t(X) %*% y)^2)
}
