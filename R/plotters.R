#' Preprocess normalized count data for PCA
#' @import matrixStats
PCA_preprocess <- function(se, assay, nfeature){
  data <- se@assays@data[[assay]]
  data <- as.matrix(data)
  data <- apply(data,c(1,2),as.numeric)
  data <- data[rowSums(data)!=0,]
  vargenes <- apply(data,1,var)
  vargenes <- vargenes[order(vargenes,decreasing = T)]
  vargenes <- vargenes[seq(1,nfeature)]
  data <- log(data+1)
  data <- data[names(vargenes),]
  data <- data+1

  # Center
  centered <- data - rowMeans(data)/rowSds(data)
  # for (i in 1:nrow(data)) {
  #   data[i,] <- (data[i,]-mean(data[i,]))/sd(data[i,])
  # }
  coldata <- data.frame(colData(se))
  PCA <- prcomp(t(centered), center=F)

  return(PCA)
}
#' This function allows you to plot PCA
#' @param se summarized experiment
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
      var_explained_df <- setNames(as.data.frame(var_explained), assay)
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
  pca_plot_data$assay <- factor(pca_plot_data$assay, levels=assays)
  plot <- ggplot(pca_plot_data,aes_string(x='PC1',y='PC2',colour=color,shape=shape,sample = 'sample'))+geom_point(size=3) + facet_wrap(vars(assay), ncol = 2, scales = 'free')
  return(list(PCA=pca_plot_data, var_explained=var_explained_data, plot=plot))
}


#' This function allows you to plot a heatmap
#' @param se summarized experiment
#' @param assay normalized or corrected
#' @param nfeature number of features to display
#' @param experiment_variable what is the experiment variable
#' @param annotation_column choose column
#' @import pheatmap
#' @return heatmap plot
#'
#' @export
heatmap_plotter <- function(se, assay, nfeature,experiment_variable,annotation_column) {
  data <- se@assays@data[[assay]]
  data <- as.matrix(data)
  data <- apply(data,c(1,2),as.numeric)
  data <- data[rowSums(data)!=0,]
  vargenes <- apply(data,1,var)
  vargenes <- vargenes[order(vargenes,decreasing = T)]
  vargenes <- vargenes[seq(1,nfeature)]
  data <- log(data+1)
  data <- data[names(vargenes),]
  data <- data+1
  for (i in 1:nrow(data)) {
    data[i,] <- (data[i,]-mean(data[i,]))/sd(data[i,])
  }

  coldata <- data.frame(colData(se))

  cor <- cor(data)
  if (!is.null(annotation_column)) {
    if (length(annotation_column)==1) {
      coldata <- data.frame(coldata[,annotation_column],row.names = rownames(coldata))
    }
    else {
      coldata <- coldata[,annotation_column]
    }
    correlation_heatmap <- pheatmap(cor,annotation_col = coldata,annotation_row = coldata,show_colnames = F,show_rownames = F
                                 ,annotation_names_col = F,annotation_names_row = F,silent = T)

    topn_heatmap <- pheatmap(data,annotation_col = coldata,show_colnames = F,annotation_names_col = F,show_rownames = F,silent = T)

    dendrogram <- topn_heatmap$tree_col

  }
  else {
    correlation_heatmap <- pheatmap(cor,show_colnames = F,show_rownames = F
                                 ,annotation_names_col = F,annotation_names_row = F,silent = T)

    topn_heatmap <- pheatmap(data,show_colnames = F,annotation_names_col = F,show_rownames = F,silent = T)

    dendrogram <- topn_heatmap$tree_col
  }


  return(list(correlation_heatmap=correlation_heatmap,
              topn_heatmap=topn_heatmap,
              dendrogram=dendrogram))
}
