#' Returns list of covariates not confounded by batch; helper function for
#' explained variation and for populating shiny app condition options
#'
#' @param se Summarized experiment object
#' @param batch Batch variable
#' @return List of explained variation by batch and condition
#' @export

covariates_not_confounded <- function(se, batch) {
    df <- confound_metrics(se, batch)
    covariate_options <- rownames(df)
    for (i in seq_len(dim(df)[1])) {
        if (df[i] == 1 || is.na(df[i])) {
            covariate_options <- covariate_options[!(covariate_options) %in%
                    rownames(df)[i]]
        }
    }
    return(covariate_options)
}
