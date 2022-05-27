globalVariables(c("value", "variable"))

#' This function allows you to plot explained variation
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @import reshape2
#' @import ggplot2
#' @return List of explained variation by batch and condition
#' @export
EV_plotter <- function(se, batch, condition, assay_name) {
  batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
  EV_boxplot <- ggplot(data=melt(as.data.frame(batchqc_ev$explained_variation),
                                 id.vars = NULL),
                       aes(x = variable, y = value, fill = variable)) +
    geom_boxplot() +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Percent Explained Variation") +
    labs(title = "Percent of Variation Explained by Source") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  return(list(EV_boxplot=EV_boxplot))
}


#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @import reshape2
#' @import ggplot2
#' @return List of explained variation by batch and condition
#' @export
covariate_pval_plotter <- function(se, batch, condition, assay_name) {
  batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
  for (i in seq_len(length(condition))) {
    names(batchqc_ev$cond_test[[i]])[1] <- condition[i]
  }
  covar_boxplot <- ggplot(subset(melt(as.data.frame(batchqc_ev$cond_test),
                                      id.vars = NULL),
                                 variable %in% condition),
                          aes(x = variable, y = value, fill = variable)) +
    geom_violin(width = 0.8) +
    geom_boxplot(width = 0.1) +
    coord_flip() +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "P-Values")+
    labs(title="Distribution of Covariate Effects (P-Values) Across Genes") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  return(list(covar_boxplot = covar_boxplot))
}


#' This function allows you to plot batch p-values of explained variation
#' @param se Summarized experiment object
#' @param batch Batch covariate
#' @param condition Condition covariate of interest
#' @param assay_name Assay of choice
#' @import reshape2
#' @import ggplot2
#' @return List of explained variation by batch and condition
#' @export
batch_pval_plotter <- function(se, batch, condition, assay_name) {
  batchqc_ev <- batchqc_explained_variation(se, batch, condition, assay_name)
  batch_boxplot <- ggplot(data = (melt(as.data.frame(batchqc_ev$batch_ps),
                                       id.vars = NULL)),
                          aes(x = variable, y = value, fill = variable)) +
    geom_violin(width = 0.8) +
    geom_boxplot(width = 0.1) +
    scale_color_manual(values = "#56B4E9", aesthetics = "fill") +
    coord_flip() +
    scale_x_discrete(name = "", labels = "Batch") +
    scale_y_continuous(name = "P-Values")+
    labs(title="Distribution of Batch Effect (P-Values) Across Genes") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  return(list(batch_boxplot = batch_boxplot))
}



#' Preprocess normalized count data for PCA
#' @param se Summarized Experiment object
#' @param assay Assay from SummarizedExperiment object
#' @param nfeature Number of variable features to use
#' @return Returns a list with class "prcomp" (see ?stats::prcomp)
#' @export
PCA_preprocess <- function(se, assay, nfeature){
  
  data <- se@assays@data[[assay]]
  data <- as.matrix(data)
  data <- apply(data, c(1, 2), as.numeric)
  data <- data[rowSums(data) != 0, ]
  vargenes <- apply(data, 1, stats::var)
  vargenes <- vargenes[order(vargenes, decreasing = TRUE)]
  vargenes <- vargenes[seq(1, nfeature)]
  data <- log(data+1)
  data <- data[names(vargenes), ]
  data <- data+1
  
  # Center
  centered <- data - rowMeans(data)/matrixStats::rowSds(data)
  # for (i in 1:nrow(data)) {
  #   data[i,] <- (data[i,]-mean(data[i,]))/sd(data[i,])
  # }
  coldata <- data.frame(colData(se))
  PCA <- stats::prcomp(t(centered), center=FALSE)
  
  return(PCA)
}



#' This function allows you to plot PCA
#' @param se SummarizedExperiment object
#' @param nfeature number of features
#' @param color choose a color
#' @param shape choose a shape
#' @param assays array of assay names from `se`
#' @import ggplot2
#' @return PCA plot
#'
#' @export
PCA_plotter <- function(se, nfeature, color, shape, assays) {
  pca_plot_data <- data.frame()
  var_explained_data <- NULL
  coldata <- data.frame(colData(se))
  for (assay in assays){
    if (! assay %in%  names(se@assays)){
      warning(sprintf('"%s" is not an available assay', assay))
      next
    }
    else {
      # Preprocess data
      pca <- PCA_preprocess(se, assay, nfeature)
      # Get variance explained
      var_explained <- summary(pca)$importance["Proportion of Variance",]
      var_explained_df <- stats::setNames(as.data.frame(var_explained), assay)
      if (is.null(var_explained_data)){
        var_explained_data <- var_explained_df
      }
      else{
        var_explained_data <- cbind(var_explained_data, var_explained_df)
      }
      
      
      # Extract PC data
      pca_data <- as.data.frame(pca$x)
      # Annotate with assay name
      pca_data['assay'] <- assay
      # Merge metadata
      pca_md <- cbind(coldata, pca_data)
      pca_md$sample <- rownames(coldata)
      # Add to data
      pca_plot_data <- rbind(pca_plot_data, pca_md)
    }
  }
  # Reorder data
  pca_plot_data$assay <- factor(pca_plot_data$assay, levels = assays)
  plot <- ggplot(pca_plot_data,
                 aes_string(x = 'PC1', y = 'PC2', colour = color,
                            shape = shape, sample = 'sample'))+
    geom_point(size = 3) + facet_wrap(vars(assay), ncol = 2, scales = 'free')
  return(list(PCA = pca_plot_data,
              var_explained = var_explained_data,
              plot = plot))
}


#' This function allows you to plot a heatmap
#' @param se SummarizedExperiment
#' @param assay normalized or corrected assay
#' @param nfeature number of features to display
#' @param annotation_column choose column
#' @import pheatmap
#' @import tibble
#' @import ggdendro
#' @import RColorBrewer
#' @import plotly
#' @import dplyr
#' @return heatmap plot
#'
#' @export
heatmap_plotter <- function(se, assay, nfeature, annotation_column) {
  data <- se@assays@data[[assay]]
  data <- as.matrix(data)
  data <- apply(data, c(1, 2), as.numeric)
  data <- data[rowSums(data) != 0,]
  
  vargenes <- apply(data, 1, stats::var)
  vargenes <- vargenes[order(vargenes, decreasing = TRUE)]
  vargenes <- vargenes[seq(1, nfeature)]
  data <- log(data+1)
  data <- data[names(vargenes),]
  data <- data+1
  for (i in seq_len(nrow(data))) {
    data[i,] <- (data[i,]-mean(data[i,]))/stats::sd(data[i,])
  }
  
  coldata <- data.frame(colData(se))
  
  cor <- cor(data)
  if (!is.null(annotation_column)) {
    if (length(annotation_column) == 1) {
      coldata <- data.frame(coldata[ , annotation_column],
                            row.names = rownames(coldata))
    }
    else {
      coldata <- coldata[, annotation_column]
    }
    correlation_heatmap <- pheatmap(cor, annotation_col = coldata,
                                    annotation_row = coldata,
                                    show_colnames = FALSE,
                                    show_rownames = FALSE,
                                    annotation_names_col = FALSE,
                                    annotation_names_row = FALSE,
                                    silent = TRUE)
    
    topn_heatmap <- pheatmap(data, annotation_col = coldata,
                             show_colnames = FALSE,
                             annotation_names_col = FALSE,
                             show_rownames = FALSE,
                             silent = TRUE)
    
    data <- se@assays@data[[assay]]
    
    data <- t(data)
    
    dat <- as.data.frame(data) %>%
      mutate(sample_name = paste("sample", seq(1:nrow(data)), sep = "_")) # create unique sample ID\
    
    rownames(dat) <- dat$sample_name
    
    sample_name <- dat$sample_name
    
    metadata <- cbind(as.data.frame(colData(se)),sample_name)
    
    metadata[] <- lapply(metadata, as.character)
    
    numeric_data <- dat 
    
    dist_matrix <- dist(numeric_data, method = "euclidean")
    dendrogram <- as.dendrogram(hclust(dist_matrix, method = "complete"))
    
    dendrogram_data <- dendro_data(dendrogram)
    dendrogram_segments <- dendrogram_data$segments
    
    dendrogram_ends <- dendrogram_segments %>%
      filter(yend == 0) %>% 
      left_join(dendrogram_data$labels, by = "x") %>% 
      rename(sample_name = label) %>%
      left_join(metadata, by = "sample_name") 
    
    unique_vars <- levels(factor(dendrogram_ends[,annotation_column])) %>% 
      as.data.frame() %>% rownames_to_column("row_id") 
    
    color_count <- length(unique(unique_vars$.))
    
    get_palette <- colorRampPalette(brewer.pal(n = length(unique(dendrogram_ends[,annotation_column])), name = "Paired"))
    
    palette <- get_palette(color_count) %>% 
      as.data.frame() %>%
      rename("color" = ".") %>%
      rownames_to_column(var = "row_id")
    color_list <- left_join(unique_vars, palette, by = "row_id") %>%
      select(-row_id)
    annotation_color <- as.character(color_list$color)
    names(annotation_color) <- color_list$.
    
    circular_dendrogram <- ggplot() +
      geom_segment(data = dendrogram_segments, 
                   aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_segment(data = dendrogram_ends,
                   aes(x=x, y=y.x, xend=xend, yend=yend, color = dendrogram_ends[,annotation_column])) +
      scale_color_manual(values = annotation_color) +
      scale_y_reverse() +
      coord_polar(theta="x") + theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      theme_bw() + ylab("Distance") 
    
    dendrogram <- ggplot() +
      geom_segment(data = dendrogram_segments, 
                   aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_segment(data = dendrogram_ends,
                   aes(x=x, y=y.x, xend=xend, yend=yend, color = dendrogram_ends[,annotation_column])) +
      scale_color_manual(values = annotation_color) +
      scale_y_reverse() +
      coord_flip() + theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      theme_bw() + ylab("Distance") 
    
  }
  else {
    correlation_heatmap <- pheatmap(cor, show_colnames = FALSE,
                                    show_rownames = FALSE,
                                    annotation_names_col = FALSE,
                                    annotation_names_row = FALSE,
                                    silent = TRUE)
    
    topn_heatmap <- pheatmap(data, show_colnames = FALSE,
                             annotation_names_col = FALSE,
                             show_rownames = FALSE,
                             silent = TRUE)
    
    data <- se@assays@data[[assay]]
    
    data <- t(data)
    
    dat <- as.data.frame(data) %>%
      mutate(sample_name = paste("sample", seq(1:nrow(data)), sep = "_")) # create unique sample ID\
    
    rownames(dat) <- dat$sample_name
    
    sample_name <- dat$sample_name
    
    metadata <- cbind(as.data.frame(colData(se)),sample_name)
    
    metadata[] <- lapply(metadata, as.character)
    
    numeric_data <- dat 
    
    dist_matrix <- dist(numeric_data, method = "euclidean")
    dendrogram <- as.dendrogram(hclust(dist_matrix, method = "complete"))
    
    dendrogram_data <- dendro_data(dendrogram)
    dendrogram_segments <- dendrogram_data$segments 
    
    dendrogram_ends <- dendrogram_segments %>%
      filter(yend == 0) %>% 
      left_join(dendrogram_data$labels, by = "x") %>% 
      rename(sample_name = label) %>%
      left_join(metadata, by = "sample_name") 
    
    unique_vars <- levels(factor(dendrogram_ends[,annotation_column])) %>% 
      as.data.frame() %>% rownames_to_column("row_id") 
    
    color_count <- length(unique(unique_vars$.))
    
    get_palette <- colorRampPalette(brewer.pal(n = length(unique(dendrogram_ends[,annotation_column])), name = "Paired"))
    
    palette <- get_palette(color_count) %>% 
      as.data.frame() %>%
      rename("color" = ".") %>%
      rownames_to_column(var = "row_id")
    color_list <- left_join(unique_vars, palette, by = "row_id") %>%
      select(-row_id)
    annotation_color <- as.character(color_list$color)
    names(annotation_color) <- color_list$.
    
    circular_dendrogram <- ggplot() +
      geom_segment(data = dendrogram_segments, 
                   aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_segment(data = dendrogram_ends,
                   aes(x=x, y=y.x, xend=xend, yend=yend, color = dendrogram_ends[,annotation_column])) +
      scale_color_manual(values = annotation_color) +
      scale_y_reverse() +
      coord_polar(theta="x") + theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      theme_bw() + ylab("Distance") 
    
    dendrogram <- ggplot() +
      geom_segment(data = dendrogram_segments, 
                   aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_segment(data = dendrogram_ends,
                   aes(x=x, y=y.x, xend=xend, yend=yend, color = dendrogram_ends[,annotation_column])) +
      scale_color_manual(values = annotation_color) +
      scale_y_reverse() +
      coord_flip() + theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      theme_bw() + ylab("Distance") 
    
  }
  
  return(list(correlation_heatmap = correlation_heatmap,
              topn_heatmap = topn_heatmap,
              circular_dendrogram = circular_dendrogram,
              dendrogram = dendrogram))
}
