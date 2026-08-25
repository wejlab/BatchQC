#' UMAP plot
#'
#' This is a wrapper function for umap package plus custom plotting. A UMAP
#' allows you to compare how tightly clustered the batch variable is in
#' comparison to the biological variable of interest. When the batch variable
#' clusters more distinctly than the biological variable, there is likely a
#' batch effect present that should be corrected.
#'
#' @param se_object se_object; containing data of interest
#' @param assay_of_interest string; the assay in the se_object to plot
#' @param batch string; representing batch
#' @param covar string; representing biological variable
#' @param neighbors integer; number of nearest neighbors, default 15 per umap;
#'   lower values prioritize local structure, higher values will represent
#'   bigger picture but lose finer details
#' @param min_distance numeric; how close points appear in final layout; higher
#'   values puts less emphasis on global structure; must be less than spread
#' @param spread numeric; dispersion of points in umap
#' @param exploratory Boolean; default is FALSE, if TRUE, a 5x5 grid with
#'   k = 15, 25, 50, 100 and min_distance = 0.1, .2, .5, .75, .99 will
#'   be plotted
#' @param log_option Boolean; default is FALSE, if TRUE,
#'   log(assay_of_interest + 1) is computed and used for plotting; useful for
#'   count data
#' @import umap
#' @return umap plot
#' @usage umap(
#'     se_object,
#'     assay_of_interest,
#'     batch,
#'     covar,
#'     neighbors = 15,
#'     min_distance = 0.1,
#'     spread = 1,
#'     exploratory = FALSE,
#'     log_option = FALSE)
#' @examples
#' library(scran)
#' se <- mockSCE()
#' se$Treatment <- as.factor(se$Treatment)
#' se$Mutation_Status <- as.factor(se$Mutation_Status)
#' umap_plot <- BatchQC::umap(se_object = se, assay_of_interest = "counts",
#' batch = "Treatment", covar = "Mutation_Status", log_option = TRUE)
#' umap_plot
#'
#' @export

umap <- function(se_object, assay_of_interest, batch, covar, neighbors = 15,
    min_distance = 0.1, spread = 1, exploratory = FALSE, log_option = FALSE) {
    plot_assay <- t(assays(se_object)[[assay_of_interest]])

    if (log_option) {
        plot_assay <- log(plot_assay + 1)
    }

    if (!exploratory) {
        umap_data <- umap::umap(plot_assay, n_neighbors = neighbors,
            min_dist = min_distance, spread = spread)
        df <- data.frame(x = umap_data$layout[, 1],
            y = umap_data$layout[, 2],
            batch = colData(se_object)[[batch]],
            covar = colData(se_object)[[covar]])

        plot <- ggplot(df, aes(x, y, color = batch, shape = covar)) +
            geom_point(size = 3)
    }else {
        if (dim(plot_assay)[1] < 15) {
            stop("Exploratory option is only valid on data sets with
                more than 15 samples.")
        }
        nearest_neighbors <- possible_k_neighbors(dim(plot_assay)[1])
        min_distance <- possible_distances(spread)
        all_plot_data <- data.frame()

        for (j in min_distance){
            for (i in nearest_neighbors){
                umap_data <- umap::umap(plot_assay, n_neighbors = i,
                    min_dist = j, spread = spread)
                df <- data.frame(x = umap_data$layout[, 1],
                    y = umap_data$layout[, 2],
                    batch = colData(se_object)[[batch]],
                    covar = colData(se_object)[[covar]],
                    num_neighbors = rep(i,
                        times = length(colData(se_object)[[batch]])),
                    min_distance = rep(j,
                        times = length(colData(se_object)[[batch]])))
                all_plot_data <- rbind(all_plot_data, df)
            }
        }

        plot <- ggplot(all_plot_data, aes(x, y, color = batch, shape = covar)) +
            geom_point() +
            facet_grid(min_distance ~ num_neighbors,
                scales = "free", as.table = FALSE)
    }
    return(plot)
}

#' Create a vector of possible nearest neighbor values from 5, 15, 25, 50,
#' and 100
#' @param data_size size of the data set used to create umaps
#' @return k nearest neighbor list

possible_k_neighbors <- function(data_size) {
    if (data_size > 100) {
        return(c(15, 25, 50, 100))
    }else if (data_size > 50) {
        return(c(15, 25, 50))
    }else if (data_size > 25) {
        return(c(15, 25))
    }else if (data_size > 15) {
        return(c(15))
    }
}

#' Create potential min_distance values for exploratory analysis based on
#' the value of spread
#' @param spread numeric; the value of spread used in the exploratory analysis
#' @return vector of min_distance values to use in exploratory analysis

possible_distances <- function(spread) {
    if (spread >= 4) {
        return(c(0.1, spread / 4, spread / 2, spread - 1, spread - 0.00001))
    }else if (spread >= 3) {
        return(c(0.1, 1, 2, 2.5, 2.9999))
    }else if (spread >= 2) {
        return(c(0.1, 0.5, 1, 1.5, 1.9999))
    }else if (spread >= 1) {
        return(c(0.1, 0.2, 0.5, 0.75, 0.9999999))
    }
}
