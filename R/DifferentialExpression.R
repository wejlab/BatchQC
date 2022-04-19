#' This function allows you to plot differential expression analysis results as a volcano plot
#' @param volcano_data a dataframe of expression change and p-value data
#' @return a volcano plot of expression change and p-value data
#' @import ggplot2
#' @export
volcano_plot = function(volcano_data,slider) {
  
  volcano_data <- as.data.frame(volcano_data)
  slider_factor <- (1 * (10^slider))
  volcano_data <- volcano_data %>%
    mutate(slider_cond = case_when(volcano_data[, 2] < slider_factor ~ "TRUE", 
                                   volcano_data[, 2] >= slider_factor ~ "FALSE", TRUE ~ 'NA'))
  
  p <- ggplot(data=volcano_data, aes(x=volcano_data[,1], y=-log10(volcano_data[,2]), color=slider_cond)) + 
    geom_point() +
    scale_color_manual(values = c('FALSE' = 'red', 'TRUE' = 'orange', 'NA'='black')) + 
    xlab("Change in Expression") +
    ylab("Signifigance Value") +
    theme(legend.position="bottom")
  
  return(p)
}

#' This function allows you to run differential expression analysis on a count matrix in the se object
#' @param se SummarizeExperiment
#' @param method Differential Expression Analysis Method
#' @param conditions DE analysis experimental design
#' @param assay_to_analyze Which assay to do DE analysis with
#' @return a data table of DE analysis results
#' @import SummarizedExperiment
#' @import scran
#' @import DESeq2
#' @export
analyze_SE = function(se,method,conditons,assay_to_analyze) {
  se=se
  data <- se@assays@data[[assay_to_analyze]]
  rownames(data) <- names(se)
  analysis_design <- as.data.frame(se@colData[conditons])
  con <- paste("~ ",colnames(analysis_design))
  
  if (method=='t') {
    res <- findMarkers(data, analysis_design[,1],
                    test.type=method, pval.type="all", lfc=1)
    res <- as.matrix(res[[1]])
    pvalue <- res[,2]
    log2FC <- res[,4]
    res <- res[order(res[,1],decreasing=FALSE),]

    to_plot <- cbind(log2FC,pvalue)
  }
  else if (method=='wilcox') {
    res <- findMarkers(data, analysis_design[,1],
                    test.type=method, pval.type="all", lfc=1)
    res <- as.matrix(res[[1]])
    pvalue <- res[,2]
    AUC <- res[,3]
    res <- res[order(res[,1],decreasing=FALSE),]
    
    to_plot <- cbind(AUC,pvalue)
  }
  else if (method=='DESeq2') {
    colnames(data) <- rownames(analysis_design)
    data[is.na(data)] <- 0
    dds <- DESeqDataSetFromMatrix(countData = abs(round(data)),
                                  colData = analysis_design,
                                  design = as.formula(con))
    res <- DESeq(dds)
    res <- results(res)
    pvalue <- res$pvalue
    log2FC <- res$log2FoldChange
    res <- as.matrix(res)
    res <- res[order(res[,6],decreasing=FALSE),]
    
    to_plot <- cbind(log2FC,pvalue)
  }
  
  return(list(res=res,
              volcano=to_plot))
}
