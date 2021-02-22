PCA_plotter <- function(se, assay, nfeature,color, shape ) {
  data=se@assays@data[[assay]]
  data=as.matrix(data)
  data=apply(data,c(1,2),as.numeric)
  data=data[rowSums(data)!=0,]
  vargenes=apply(data,1,var)
  vargenes=vargenes[order(vargenes,decreasing = T)]
  vargenes=vargenes[seq(1,nfeature)]
  data=log(data+1)
  data=data[names(vargenes),]
  data=data+1
  for (i in 1:nrow(data)) {
    data[i,]=(data[i,]-mean(data[i,]))/sd(data[i,])
  }

  coldata=data.frame(colData(se))
  PCA=prcomp(t(data))
  coldata=cbind(coldata,PCA$x)
  coldata$sample=rownames(coldata)

  plot=ggplot(coldata,aes_string(x='PC1',y='PC2',colour=color,shape=shape,sample = 'sample'))+geom_point(size=3)
  return(list(PCA=PCA,plot=plot))
}



heatmap_plotter <- function(se, assay, nfeature,experiment_variable,annotation_column) {
  data=se@assays@data[[assay]]
  data=as.matrix(data)
  data=apply(data,c(1,2),as.numeric)
  data=data[rowSums(data)!=0,]
  vargenes=apply(data,1,var)
  vargenes=vargenes[order(vargenes,decreasing = T)]
  vargenes=vargenes[seq(1,nfeature)]
  data=log(data+1)
  data=data[names(vargenes),]
  data=data+1
  for (i in 1:nrow(data)) {
    data[i,]=(data[i,]-mean(data[i,]))/sd(data[i,])
  }

  coldata=data.frame(colData(se))

  cor=cor(data)
  if (!is.null(annotation_column)) {
    coldata=coldata[,unique(c(experiment_variable,'Batch',annotation_column))]
  }else {
    coldata=coldata[,unique(c(experiment_variable,'Batch'))]
  }
  correlation_heatmap=pheatmap(cor,annotation_col = coldata,annotation_row = coldata,show_colnames = F,show_rownames = F
                               ,annotation_names_col = F,annotation_names_row = F,display_numbers = T,silent = T)

  topn_heatmap = pheatmap(data,annotation_col = coldata,show_colnames = F,annotation_names_col = F,show_rownames = F,silent = T)

  dendrogram=topn_heatmap$tree_col

  return(list(correlation_heatmap=correlation_heatmap,
              topn_heatmap=topn_heatmap,
              dendrogram=dendrogram))
}
