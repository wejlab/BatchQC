#' kBET rejection rate
#' @description This function runs the k-nearest neighbor batch effect test
#' (kBET) to evaluate whether the data has detectable batch effect.
#'
#' @param se SummarizedExperiment object
#' @param assay_to_normalize string; assay from se object to do normalization
#' @param batch character string of column name that represents batch
#' @param k0 integer representing number of nearest neighbors to test on
#' (neighborhood size)
#' @param knn n x k matrix of nearest neighbors for each cell (optional)
#' @param testSize integer representing number of data points to test
#' @param do.pca Boolean, if TRUE, perform a pca prior to knn search
#' (defaults to TRUE)
#' @param dim.pca Boolean, if do.pca=TRUE, choose the number of dimensions to
#' consider (defaults to 50)
#' @param heuristic Boolean, if true, compute an optimal neighborhood size k
#' (defaults to TRUE)
#' @param n_repeat numeric representing 'n_repeat' subsets to evaluate in order
#' to create a statistics on batch estimates
#' @param alpha numeric for significance level
#' @param addTest Boolean, if TRUE, perform an LRT-approximation to the
#' multinomial test AND a multinomial exact test (if appropriate)
#' @param verbose Boolean, if TRUE, display stages of current computation
#' (defaults to FALSE)
#' @param adapt Boolean, if TRUE, frequencies will be adapted (defaults to TRUE)
#'
#' @returns list object from kBET() function
#'    \enumerate{
#'    \item \code{summary} - a rejection rate for the data,
#'         an expected rejection rate for random
#'         labeling and the significance for the observed result
#'    \item \code{results} - detailed list for each tested cells;
#'         p-values for expected and observed label distribution
#'    \item \code{average.pval} - significance level over the averaged
#'         batch label distribution in all neighbourhoods
#'    \item \code{stats} - extended test summary for every sample
#'    \item \code{params} - list of input parameters and adapted parameters,
#'    respectively
#'    \item \code{outsider} - only shown if \code{adapt=TRUE}. List of samples
#'         without mutual nearest neighbour: \itemize{
#'     \item \code{index} - index of each outsider sample)
#'     \item \code{categories} - tabularised labels of outsiders
#'     \item \code{p.val} - Significance level of outsider batch label
#'         distribution vs expected frequencies.
#'     If the significance level is lower than \code{alpha},
#'     expected frequencies will be adapted}
#'    }
#' @import SummarizedExperiment
#' @import ggplot2
#'
#' @usage run_kBET(
#'     se,
#'     assay_to_normalize,
#'     batch,
#'     k0 = NULL,
#'     knn = NULL,
#'     testSize = NULL,
#'     do.pca = TRUE,
#'     dim.pca = 50,
#'     heuristic = TRUE,
#'     n_repeat = 100,
#'     alpha = 0.05,
#'     addTest = FALSE,
#'     verbose = FALSE,
#'     adapt = TRUE)
#' @examples
#' library(scran)
#' se <- mockSCE()
#' kBET_result <- BatchQC::run_kBET(
#'     se = se,
#'     assay_to_normalize = "counts",
#'     batch = "Treatment"
#' )
#'
#' BatchQC::plot_kBET(kBET_result)
#'
#' @export
run_kBET <- function(
    se, assay_to_normalize, batch, k0 = NULL, knn = NULL,
    testSize = NULL, do.pca = TRUE, dim.pca = 50, heuristic = TRUE,
    n_repeat = 100, alpha = 0.05, addTest = FALSE, verbose = FALSE,
    adapt = TRUE) {
    # run kBET
    batch.estimate <- kBET(
        df = as.matrix(assays(se)[[assay_to_normalize]]),
        batch = data.frame(colData(se))[, batch],
        k0 = k0, knn = knn,
        testSize = testSize, do.pca = do.pca,
        dim.pca = dim.pca, heuristic = heuristic,
        n_repeat = n_repeat, alpha = alpha,
        addTest = addTest, verbose = verbose,
        plot = FALSE, adapt = adapt
    )

    return(batch.estimate)
}

#' kBET - k-nearest neighbour batch effect test
#'
#' @description adapted from kBET package (https://github.com/theislab/kBET).
#' \code{kBET} runs a chi square test to evaluate
#' the probability of a batch effect.
#' @importFrom FNN get.knn
#' @import ggplot2
#' @import tidyverse
#' @importFrom stats quantile pchisq
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils data
#' @importFrom methods is
#' @include kBET-utils.R
#' @name kBET
#' @param df dataset (rows: cells, columns: features)
#' @param batch batch id for each cell or a data frame with
#' both condition and replicates
#' @param k0 number of nearest neighbours to test on (neighbourhood size)
#' @param knn an n x k matrix of nearest neighbours for each cell (optional)
#' @param testSize number of data points to test,
#' (10 percent sample size default, but at least 25)
#' @param do.pca perform a pca prior to knn search? (defaults to TRUE)
#' @param dim.pca if do.pca=TRUE, choose the number of dimensions
#' to consider (defaults to 50)
#' @param heuristic compute an optimal neighbourhood size k
#' (defaults to TRUE)
#' @param n_repeat to create a statistics on batch estimates,
#' evaluate 'n_repeat' subsets
#' @param alpha significance level
#' @param adapt In some cases, a number of cells do not contribute
#' to any neighbourhood
#' and this may cause an imbalance in observed and expected batch
#' label frequencies.
#' Frequencies will be adapted if adapt=TRUE (default).
#' @param addTest perform an LRT-approximation to the multinomial
#' test AND a multinomial exact test (if appropriate)
#' @param plot if stats > 10, then a boxplot of the resulting
#' rejection rates is created
#' @param verbose displays stages of current computation (defaults to FALSE)
#' @usage kBET(
#'     df,
#'     batch,
#'     k0 = NULL,
#'     knn = NULL,
#'     testSize = NULL,
#'     do.pca = TRUE,
#'     dim.pca = 50,
#'     heuristic = TRUE,
#'     n_repeat = 100,
#'     alpha = 0.05,
#'     addTest = FALSE,
#'     verbose = FALSE,
#'     plot = TRUE,
#'     adapt = TRUE)
#' @return list object
#'    \enumerate{
#'    \item \code{summary} - a rejection rate for the data,
#'         an expected rejection rate for random
#'         labeling and the significance for the observed result
#'    \item \code{results} - detailed list for each tested cells;
#'         p-values for expected and observed label distribution
#'    \item \code{average.pval} - significance level over the averaged
#'         batch label distribution in all neighbourhoods
#'    \item \code{stats} - extended test summary for every sample
#'    \item \code{params} - list of input parameters and adapted parameters,
#'    respectively
#'    \item \code{outsider} - only shown if \code{adapt=TRUE}. List of samples
#'         without mutual nearest neighbour: \itemize{
#'     \item \code{index} - index of each outsider sample)
#'     \item \code{categories} - tabularised labels of outsiders
#'     \item \code{p.val} - Significance level of outsider batch label
#'         distribution vs expected frequencies.
#'     If the significance level is lower than \code{alpha},
#'     expected frequencies will be adapted}
#'    }
#' @return If the optimal neighbourhood size (k0) is smaller than 10, NA is
#' returned.
#' @examples
#' library(scran)
#' se <- mockSCE()
#' df <- as.matrix(assays(se)[["counts"]])
#' batch <- data.frame(colData(se))[, "Treatment"]
#'
#' batch.estimate <- kBET(df, batch)
#'
#' @export
kBET <- function(
    df, batch, k0 = NULL, knn = NULL, testSize = NULL, do.pca = TRUE,
    dim.pca = 50, heuristic = TRUE, n_repeat = 100, alpha = 0.05,
    addTest = FALSE, verbose = FALSE, plot = TRUE, adapt = TRUE) {
    # preliminaries:
    initialize.kbet.res <- initialize.kbet(
        df, batch, k0, knn, testSize, do.pca, dim.pca, heuristic, n_repeat,
        alpha, addTest, verbose, adapt
    )
    if (addTest) {
        kbet.res <- run.kbet.addTest(
            initialize.kbet.res, adapt, alpha, n_repeat
        )
        if (n_repeat > 1 & plot) {
            plot_kbet_helper(
                kbet.res$kBET.observed, kbet.res$kBET.expected,
                kbet.res$lrt.observed, kbet.res$lrt.expected,
                kbet.res$exact.observed, kbet.res$exact.expected, n_repeat)
        }
    } else { # kBET only
        kbet.res <- run.kbet.only(initialize.kbet.res, adapt, alpha, n_repeat)
        if (n_repeat > 1 & plot) {
            plot_kbet_helper(
                kbet.res$kBET.observed, kbet.res$kBET.expected,
                n_repeat = n_repeat)
        }
    }
    rejection <- summarize_kbet_results(
        kbet.res$rejection, kbet.res$kBET.expected, kbet.res$kBET.observed,
        kbet.res$kBET.signif, kbet.res$lrt.expected, kbet.res$lrt.observed,
        kbet.res$lrt.signif, kbet.res$exact.observed, kbet.res$exact.expected,
        kbet.res$exact.signif, n_repeat, addTest)
    # collect parameters
    rejection$params <- list(
        k0 = k0, testSize = testSize, do.pca = do.pca, dim.pca = dim.pca,
        heuristic = heuristic, n_repeat = n_repeat, alpha = alpha,
        addTest = addTest, verbose = verbose, plot = plot
    )
    # add outsiders
    if (adapt) {
        rejection$outsider <- list(
            index = initialize.kbet.res$outsider,
            categories = table(batch[initialize.kbet.res$outsider]),
            p.val = initialize.kbet.res$p.out
        )
    }
    rejection
}

#' bisect - a generic bisection function
#' @description adapted from kBET package (https://github.com/theislab/kBET).
#' Provides recursive bisection algorithm for an arbitrary function. It
#' evaluates the function \code{foo} at the bounds and replaces one of the
#' boundaries until a maximum is found or the interval becomes too small
#'
#' @param foo a function mapping a one-dim argument to one-dim value
#'
#' @param bounds a vector of length 2 with real valued numbers
#'     (i.e. two arguments of \code{foo})
#' @param known tells for which of the arguments a value is known
#'     (defaults to NULL)
#' @param ... additional parameters for \code{foo}
#' @param tolx break condition for argument (defaults to 10)
#' @param toly break condition for value (defaults to 0.01)
#' @return A range of bounds where \code{foo} is maximal.
#' @importFrom stats dist
#'
#' @examples
#' get_maximum <- bisect(function(x) {
#'     -(x - 2)^2
#' }, c(-5, 50))
#'
#' @export
bisect <- function(foo, bounds, known = NULL, ..., tolx = 5, toly = 0.01) {
    if (is.null(known)) {
        evalFoo <- vapply(bounds, foo, ..., FUN.VALUE = numeric(1))
        if (diff(unlist(evalFoo)) < -toly && diff(bounds) > tolx) {
            bounds[2] <- round(sum(bounds) / 2, 0)
            known <- c(unlist(evalFoo)[1], 0)
            bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
        } else if (diff(unlist(evalFoo)) > toly && diff(bounds) > tolx) {
            bounds[1] <- round(sum(bounds) / 2, 0)
            known <- c(0, unlist(evalFoo)[2])
            bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
        } else if (dist(unlist(evalFoo)) < toly) {
            center <- vapply(
                round(sum(bounds) / 2, 0), foo, ..., FUN.VALUE = numeric(1)
            )
            dist_center <- vapply(
                unlist(evalFoo), function(x, y) {
                    dist(c(x, y))}, center, FUN.VALUE = numeric(1))
            if (max(abs(dist_center)) < toly || dist(bounds) < tolx) {
                bounds  # return interval and stop recursion
            } else if (dist_center[1] > toly) {
                bounds[2] <- round(sum(bounds) / 2, 0)
                known <- c(unlist(evalFoo)[1], 0)
                bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
            } else if (dist_center[2] > toly) {
                bounds[1] <- round(sum(bounds) / 2, 0)
                known <- c(0, unlist(evalFoo)[2])
                bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
            }
        }
    } else {
        new.eval <- which(known == 0)
        old.eval <- which(known != 0)
        evalFoo <- vapply(bounds[new.eval], foo, ..., FUN.VALUE = numeric(1))
        result <- numeric(length(known))
        result[new.eval] <- unlist(evalFoo)
        result[old.eval] <- known[old.eval]
        if (diff(result) < -toly && diff(bounds) > tolx) {
            bounds[2] <- round(sum(bounds) / 2, 0)
            known <- c(unlist(evalFoo)[1], 0)
            bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
        } else if (diff(result) > toly && diff(bounds) > tolx) {
            bounds[1] <- round(sum(bounds) / 2, 0)
            known <- c(0, unlist(evalFoo)[1])
            bisect(foo, bounds, known, ..., tolx = tolx, toly = toly)
        } else if (dist(result) < toly || dist(bounds) < tolx) {
            bounds  # return interval and stop recursion
        }
    }
}

#' kBET Rejection Plotter
#' @description This function generates a boxplot of observed and expected
#' rejection rates for the provided kBET output list object
#'
#' @param kBET_res list object output from kBET function
#'
#' @returns ggplot object containing kBET rejection boxplot
#'
#' @examples
#' library(scran)
#' se <- mockSCE()
#' df <- as.matrix(assays(se)[["counts"]])
#' batch <- data.frame(colData(se))[, "Treatment"]
#'
#' batch.estimate <- kBET(df, batch)
#' plot_kBET(batch.estimate)
#'
#' @export
plot_kBET <- function(kBET_res) {
    # create ggplot object for plotting kBET's rejection rate
    plot.data <- data.frame(
        class = rep(c("observed", "expected"),
            each = length(kBET_res$stats$kBET.observed)
        ),
        data = c(
            kBET_res$stats$kBET.observed,
            kBET_res$stats$kBET.expected
        )
    )
    g <- ggplot(plot.data, aes(class, data)) +
        geom_boxplot(aes(fill = class)) +
        labs(x = "Test", y = "Rejection rate", title = "kBET test results") +
        theme_bw() +
        theme(legend.position = "none") +
        scale_y_continuous(limits = c(0, 1))

    return(g)
}
