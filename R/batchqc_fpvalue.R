#' Returns R2 values from F-test (full/reduced model) for explained
#' variation plot and table
#'
#' @param se Summarized experiment object
#' @param mod mod
#' @param batch_mod mod
#' @param assay_name Name of chosen assay
#' @return List of explained variation by batch and condition
#' @export
batchqc_f.pvalue <- function(se, mod, batch_mod, assay_name) {
    mod00 <- matrix(rep(1, ncol(se)), ncol = 1)
    n <- dim(se)[2]
    m <- dim(se)[1]
    df1 <- dim(mod)[2]
    df0 <- dim(batch_mod)[2]
    p <- rep(0, m)

    resid <- as.matrix(assays(se)[[assay_name]]) - as.matrix(
        assays(se)[[assay_name]]) %*% mod %*%
        solve(t(mod) %*% mod) %*% t(mod)
    rss1 <- rowSums(resid * resid)
    rm(resid)

    resid0 <- as.matrix(assays(se)[[assay_name]]) - as.matrix(
        assays(se)[[assay_name]]) %*%
        batch_mod %*%
        solve(t(batch_mod) %*% batch_mod) %*%
        t(batch_mod)
    rss0 <- rowSums(resid0 * resid0)
    rm(resid0)

    resid00 <- as.matrix(assays(se)[[assay_name]]) - as.matrix(
        assays(se)[[assay_name]]) %*%
        mod00 %*%
        solve(t(mod00) %*% mod00) %*%
        t(mod00)
    rss00 <- rowSums(resid00 * resid00)
    rm(resid00)

    r2_full <- 1 - rss1 / rss00
    r2_reduced <- 1 - rss0 / rss00

    p <- 1
    if (df1 > df0)  {
        fstats <- ((rss0 - rss1) / (df1 - df0)) / (rss1 / (n - df1))
        p <- 1 - stats::pf(fstats, df1 = (df1 - df0), df2 = (n - df1))
    }
    return(list(p = p, r2_full = r2_full, r2_reduced = r2_reduced))
}
