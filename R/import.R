# Functions for importing data and summarizing experimental design
library(SummarizedExperiment)
library(dplyr)
library(tidyr)

summarize_experiment = function(Counts_path,metadata_path) {
  require(reader)
  coldata=read.table(metadata_path,header = T,row.names = 1,check.names = F,sep = get.delim(metadata_path,n = 10,delims = c('\t',',')))
  counts=read.table(Counts_path,header = T,row.names = 1,check.names = F,sep = get.delim(Counts_path,n = 10,delims = c('\t',',')))
  counts=counts[rowSums(counts)>0,]
  mutual_sample=intersect(colnames(counts),rownames(coldata))
  counts=counts[,mutual_sample]
  coldata=coldata[mutual_sample,]

  se = SummarizedExperiment(assay=list(counts=counts
  ), colData=coldata)
}
ingest_data <- function(se,group,batch){
  require(SummarizedExperiment)
  require(EBSeq)

  if (!is.null(se)) {
    variables=colnames(colData(se))
    covs=variables[!variables%in%c(batch,group)]
    colnames(colData(se))[colnames(colData(se))==batch]='Batch'

    # Add covariates
    metadata(se)$covariates <- covs
    # Add experimental group variable
    metadata(se)$Experimental_group=group

    # Get counfounding metrics
    metadata(se)$confound.metrics <- confound_metrics(se)
    # Calculate CPM normalization for the summarizeexperiment
    se@assays@data$CPM=((se@assays@data$counts+1)/colSums(se@assays@data$counts))*(10^6)
    # Calculate Median of Ratio normalization for the summarizeexperiment
    require(EBSeq)
    se@assays@data$DESEQ_Method=GetNormalizedMat(se@assays@data$counts, MedianNorm(se@assays@data$counts))
    # EdgeR won't go straight-forward on how exactly they do their normalization, so I will just pass here.
    colData(se)$library_size=colSums(se@assays@data$counts)

  }
  else {
    se = NULL
  }
  return(se)
}

batch_design <- function(se, covariate){
  #' Create a batch design table for the provided covariate
  design <- colData(se) %>% as_tibble %>% group_by(eval(as.symbol(covariate))) %>% count(Batch) %>% pivot_wider(names_from = Batch, values_from = n)
  names(design)[names(design) == "eval(as.symbol(covariate))"] <- ""
  for (i in 2:length(design)) {
    colnames(design)[i] <- paste("Batch",i-1)
  }
  return(design)
}


  out = list("chi" = chi, "mmin"=mmin, "tablesum"=tablesum)
  return(out)
}

std_pearson_corr_coef <- function(bd) {
  #' Calculate standardized Pearson correlation coefficient
  c <- cor_props(bd)
  r <- sqrt(c$chi * c$mmin/((c$chi + c$tablesum) * (c$mmin - 1)))
  return(r)
}

cramers_v <- function(bd) {
  # Calculate Cramer's V
  c <- cor_props(bd)
  v <- sqrt(c$chi/(c$tablesum * (c$mmin - 1)))
  return(v)
}

confound_metrics <- function(se){
  covs = metadata(se)$covariates
  metrics <- list("Pearson Correlation Coefficient"=std_pearson_corr_coef, "Cramer's V"=cramers_v)
  metric.mat <- matrix(nrow=length(covs), ncol=length(metrics), dimnames = list(covs, names(metrics)))

  for (c in covs){
    # Get batch design
    bd <- batch_design(se, c)
    for (m in names(metrics)){
      # Compute metric and place in appropriate slot
      metric.mat[c, m] <- metrics[[m]](bd)
    }
  }
  # Add metrics to se
  return(metric.mat)
}

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
    if (length(annotation_column)==1) {
      coldata=data.frame(coldata[,annotation_column],row.names = rownames(coldata))
    }
    else {
      coldata=coldata[,annotation_column]
    }
    correlation_heatmap=pheatmap(cor,annotation_col = coldata,annotation_row = coldata,show_colnames = F,show_rownames = F
                                 ,annotation_names_col = F,annotation_names_row = F,silent = T)

    topn_heatmap = pheatmap(data,annotation_col = coldata,show_colnames = F,annotation_names_col = F,show_rownames = F,silent = T)

    dendrogram=topn_heatmap$tree_col

  }
  else {
    correlation_heatmap=pheatmap(cor,show_colnames = F,show_rownames = F
                                 ,annotation_names_col = F,annotation_names_row = F,silent = T)

    topn_heatmap = pheatmap(data,show_colnames = F,annotation_names_col = F,show_rownames = F,silent = T)

    dendrogram=topn_heatmap$tree_col
  }


  return(list(correlation_heatmap=correlation_heatmap,
              topn_heatmap=topn_heatmap,
              dendrogram=dendrogram))
}
