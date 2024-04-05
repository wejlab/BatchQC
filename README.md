
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BatchQC

## Introduction

Sequencing and microarray samples are often collected or processed in
multiple batches or at different times. This often produces technical
biases that can lead to incorrect results in the downstream analysis.
BatchQC is a software tool that streamlines batch preprocessing and
evaluation by providing interactive diagnostics, visualizations, and
statistical analyses to explore the extent to which batch variation
impacts the data. BatchQC diagnostics help determine whether batch
adjustment needs to be done, and how correction should be applied before
proceeding with a downstream analysis. Moreover, BatchQC interactively
applies multiple common batch effect approaches to the data, and the
user can quickly see the benefits of each method. BatchQC is developed
as a Shiny App. The output is organized into multiple tabs, and each tab
features an important part of the batch effect analysis and
visualization of the data. The BatchQC interface has the following
analysis groups: Upload Data, Experimental Design, Variation Analysis,
Heatmaps, Dendrograms, PCA Analysis, Differential Expression Analysis,
and Data Download.

The package includes:

1.  Upload Data; apply desired Normalization and Batch Effect Correction
    (incl.  ComBat and ComBat-Seq)
2.  Experimental Design to view summary of data
3.  Variation Analysis
4.  Heatmap plot of gene expressions
5.  Median Correlation Plot
6.  Circular Dendrogram clustered and colored by batch, condition, and
    covariates
7.  Principal Component Analysis and plots
8.  Differential Expression Plots and Analysis using DESeq2
9.  Data Download to export any corrections or normalizations as an SE
    object

`BatchQC()` is the function that launches the Shiny App in interactive
mode.

## Installation

### Bioconductor Version

When pushed to Bioconductor (aka not yet possible, this will download
the old version of BatchQC): To begin, install
[Bioconductor](http://www.bioconductor.org/) and then install BatchQC:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("BatchQC")
```

### Github Version

To install the most up-to-date version of BatchQC, please install
directly from github. You will need the devtools package. You can
install both of these with the following commands:

``` r
if (!require("devtools", quietly = TRUE)) {
    install.packages("devtools")   
}
library(devtools)
install_github("wejlab/BatchQC")
```

### Load BatchQC and Launch Shiny App

You should now be able to load BatchQC and launch the shiny app.
