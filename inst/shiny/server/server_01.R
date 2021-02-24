#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(SummarizedExperiment)
source("../../R/import.R")


### Ingest user data
#observe({
#    # Look for user file upload
#    if (!is.null(input$counts) & !is.null(input$md)){
#        se <<- ingest_data(input$counts$datapath, input$md$datapath)
#    }
#   else if (!is.null(input$se)){
#        se <<- SummarizedExperiment(input$se$datapath)
#    }
#    else {
#        se <<- NULL
#    }
#    # Populate drop down menu with covariates
#    req(se)
#    covs <- metadata(se)$covariates
#    updateSelectInput(inputId = "covariate", choices = covs)
#})

### Create batch design table when covariate selected
observeEvent(input$covariate, {
    req(se)
    output$summaryTable <- renderTable({
        bd <<- batch_design(se, input$covariate)
    })
})

# Compute counfounding design
observeEvent(input$covariate, {
    req(se)
    output$confoundingTable <- renderTable({
        metadata(se)$confound.metrics
    }, rownames = T)
})
observeEvent( input$md, {
    if (is.null(input$md)) return()
    temp=input$md
    updateSelectInput(session,inputId = "group", choices = (colnames(temp)),selected = NULL)
    updateSelectInput(session,inputId = "batch", choices = (colnames(temp)),selected = NULL)

})
