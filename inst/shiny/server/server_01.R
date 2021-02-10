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

# Define server logic

    output$summaryTable <- renderTable({
        ## INGEST USER INPUT
        if (!is.null(input$counts) & !is.null(input$counts)){
            se <- ingest_data(input$counts$datapath, input$md$datapath)
        }
        else if (!is.null(input$se)){
            ### THIS NEEDS TO BE TESTED
            se <- SummarizedExperiment(input$se$datapath)
        }
        else {
            se <- NULL
        }
        ## Display batch design
        req(se) ### The following should only run once se is defined
        # Get covariate names
        cols = names(colData(se))
        covs = names[names != 'Batch']
        updateSelectInput(inputId = "covariate", choices = covs)
        bd <- batch_design(se, input$covariate)
        return(bd)

    })
