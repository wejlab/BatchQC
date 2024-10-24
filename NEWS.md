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
