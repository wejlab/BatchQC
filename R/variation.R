#' Returns a list of explained variation by batch and condition
#' combinations
#'
#' @param se Summarized experiment object
#' @param condition Condition covariate of interest
#' @param batch Batch covariate
#' @return List of explained variation by batch and condition
#' @export
batchqc_explained_variation <- function(se, condition, batch) {
  df <- se@colData
  # batch_data <- df[names(df)==batch]
  # covariate_data <- df[names(df)==condition]
  nlb <- n_distinct(as.data.frame(df[batch]))
  nlc <- n_distinct(as.data.frame(df[condition]))
  if ((nlb <= 1)&&(nlc <= 1))  {
    cond_mod <- matrix(rep(1, ncol(se)), ncol = 1)
    batch_mod <- matrix(rep(1, ncol(se)), ncol = 1)
  } else if(nlb <= 1)  {
    cond_mod <- model.matrix(~as.factor(condition))
    batch_mod <- matrix(rep(1, ncol(se)), ncol = 1)
  } else if(nlc <= 1)  {
    cond_mod <- matrix(rep(1, ncol(se)), ncol = 1)
    batch_mod <- model.matrix(~as.factor(batch))
  } else {
    cond_mod <- model.matrix(~as.factor(condition))
    batch_mod <- model.matrix(~as.factor(batch))
  }
  mod <- cbind(cond_mod, batch_mod[, -1])
  #
  # cond_test <- batchqc_f.pvalue(se, mod, batch_mod)
  # batch_test <- batchqc_f.pvalue(se, mod, cond_mod)
  #
  # cond_ps <- cond_test$p
  # batch_ps <- batch_test$p
  #
  # r2_full <- cond_test$r2_full
  # cond_r2 <- batch_test$r2_reduced
  # batch_r2 <- cond_test$r2_reduced
  # explained_variation <- round(cbind(`Full (Condition+Batch)` = r2_full,
  #                                    Condition = cond_r2, Batch = batch_r2), 5) * 100
  # rownames(explained_variation) <- rownames(data.matrix)
  # batchqc_ev <- list(explained_variation = explained_variation,
  #                    cond_test = cond_test, batch_test = batch_test)
  #
  # return(batchqc_ev)
}
