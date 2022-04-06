# Basic structure test:
## reactive value: pre-determined name:
options(shiny.maxRequestSize=600*1024^2)
library(SummarizedExperiment)
#source("../../R/import.R")

reactivevalue=reactiveValues(counts=NULL,
                               counts_location=NULL,
                               metadata='',
                               metadata_location=NULL,
                               se_location=NULL,
                               se=NULL)

source('server/observer.R',local = T)

#output$group_variable_Name=renderText({reactivevalue$group_variable_Name})
output$confounding_table=NULL
output$metadata=NULL







