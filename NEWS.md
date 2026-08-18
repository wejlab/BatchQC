# Version 2.8.1
## Minor Changes
* Updated discrepancy in roxygen comment for lambda stat
* Added headers for all vignette code chunks
* Corrected spacing and line indent issues for a number of man files

# Version 2.7.2
## Minor Changes
* Changed maintainer from Jessica Anderson to Yaoan Leng

# Version 2.7.2
## Minor Changes
* Added lambda example to Intro vignette

# Version 2.5.22
## Major Changes
* Updated AIC GoF to run lognormal and voom for nondiscrete data
* Added a boxplot of the distribution of aic values to aic_compute
* Added AIC_median value
* Updated compute_aic to return a dataframe (rather than multiple) and a boxplox

## Minor Changes
* Added Harman description to shiny interface
* Added checkbox for advanced options for Harman

# Version 2.5.21
## Major Changes
* Added Harman method in batch correction

# Version 2.5.20
## Minor Changes
* Updated AIC to provide an overall metric

# Version 2.5.19
## Minor Changes
* Added color to the generated kBET plots

# Version 2.5.18
## Minor Changes
* Added summary statistics and p-values for explained variation boxplots

# Version 2.5.17
## Major Changes
* Added edgeR negative binomial tests

## Minor Changes
* Updated non-parametric (large sample) thresholds and adjusted p-value info

# Version 2.5.16

## Minor Changes
* Updated negative binomial test to allow users to set the low sample threshold
* Created place holders in negative binomial function for edgeR usability
* Increased umap point size

# Version 2.5.15

## Minor Changes
* Added "log_option" to UMAP to allow seamless UMAP plotting for counts data

## Bug Fixes
* Reworked dendrogram_alpha_numeric_check to use is.numeric() instead of
  as.numeric() to avoid warning due to coercion 

# Version 2.5.14
## Minor Changes
* Added voom as a normalization method

## Bug Fixes
* Corrected limma correction to allow 1 covariate

# Version 2.5.13
## Major Changes
* Updated Vignette to reflect updates to BatchQC (distribution checks, kBET, UMAP)
* Updated vignette example data to be TB data

## Minor Changes
* Updated UMAP example to include covar option
* Changed location of kBET tab

# Version 2.5.12
## Minor Changes
* Fixed sva batch correction function
* Added covar to UMAP

# Version 2.5.11
## Major Changes
* Allowed AIC function to take multiple covariates as input

# Version 2.5.10
## Major Changes
* Added a new kBET tab for measuring batch effect presence

# Version 2.5.9
## Major Changes
* Added AIC under batch correction tab

# Version 2.5.8
## Major Changes
* Added svaseq as a batch correction method

# Version 2.5.7
## Minor Changes
* Added a data onject with subject IDs and BMI info to match the original study
  with the curatedTBData info and provide missing BMI info
  
# Version 2.5.6
## Major Changes
* Added Kruskal-Wallis test as a differential expression analysis method

# Version 2.5.5
## Major Changes
* Added ANOVA as a differential expression analysis method

# Version 2.5.4
## Major Changes
* Redesigned tab flow; upload tab reduced, batch correction tab created, and
  lambda stat and neg binomial distribution check moved to appropriate tabs

## Minor Changes
* Updated lambda statistic to provide a recommendation and to check for balanced
  experimental design
* Abbreviated Normalization usage description

## Bug Fix
* Updated "counts" language to "assay" for uploaded data

# Version 2.5.3
## Major Changes
* Added lambda statistic

## Bug Fix
* Corrected log to be log(x+1) and changed CPM(x+1) to CPM(x)

# Version 2.5.2
## Major Changes
* Added edgeR as a normalization method
* Added edgeR as a differential expression analysis method

# Version 2.5.1
## Major Changes
* Added TB data example

## Minor Changes
* Changed sapply to vapply in nb check

# Version 2.2.5
## Minor Changes
* Added sva batch correction method (for unknown variation correction)

# Version 2.2.4
## Major Changes
* Updated variation ratio analysis to be log transformed
* Added umap exploratory option

# Version 2.2.3
## Major Changes
* Added limma as a batch correction
* Added umap plot option
* Added ellipses to pca plot

## Minor Changes
* Set p-val plot x scale to always be 0 to 1
* Removed Intercept from p-val violin plots

# Version 2.2.2
## Minor Changes
* Updated SE object upload to allow assays of any name (no longer require one
  assay to be called "counts")
  
# Version 2.2.1
## Minor Changes
* Added pval information to the DESeq2 binomial evaluation

# Version 2.1.6
## Bug Fixes
* Corrected code for proper division of less than or 20+ samples

#Version 2.1.5
## bug Fixes
* Coerce variable of interest to a factor

# Version 2.1.4
## Major Changes
* Added negative binomial check for 20+ samples to DESeq2

# Version 2.1.3
## Major Changes
* Added negative binomial check for less than 20 samples to DESeq2

# Version 2.1.2
## Major Changes
* Added Variation Ratio Statistic to the explained variation tab

## Minor Changes
* Removed extra "Samples" column from example data
* Uploaded bladder example data batch variable as a factor

# Version 2.1.1
## Bug Fixes
* Updated imports to include shinyjs
* Updated imports to remove dendextend which is no longer utilized
* Corrected typos in Intro vignette

# Version 2.0.0

## Bug Fixes
* Fixed various errors for the Bioconductor build

## Major Changes
* Created interactive Shiny interface
* Allow user upload of data
* Allow user download of Batch corrected and/or normalized data
* Added Example Data Functionality

## Minor Changes
* Added a `NEWS.md` file to track changes to the package.

## Changes made to dendrogram
* Added `dendrogram_color_palette.R` for coloring dendrogram
* Updated `dendrogram.R` allowing batch & category to plot together
