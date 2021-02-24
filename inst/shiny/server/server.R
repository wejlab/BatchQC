# Basic structure test:
## reative value: pre-determined name:
options(shiny.maxRequestSize=600*1024^2)
library(SummarizedExperiment)
source("../../R/import.R")




reactivevalue=reactiveValues(counts=NULL,
                               counts_location=NULL,
                               metadata='',
                               metadata_location=NULL,
                               se_location=NULL,
                               batch_Variable_Name=NULL,
                               group_variable_Name=NULL,
                               covariates=NULL,
                               se=NULL)

source('server/observer.R',local = T)

#output$group_variable_Name=renderText({reactivevalue$group_variable_Name})
output$confoundingTable=NULL
output$metadata=NULL







