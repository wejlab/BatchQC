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


    observe({
        if (!is.null(input$counts) & !is.null(input$md)){
            se <- ingest_data(input$counts$datapath, input$md$datapath)
        }
        else if (!is.null(input$se)){
            ### THIS NEEDS TO BE TESTED
            se <- SummarizedExperiment(input$se$datapath)
        }
        else {
            se <- NULL
        }
        req(se)
        cols <- names(colData(se))
        covs <- cols[cols != 'Batch']
        updateSelectInput(inputId = "covariate", choices = covs)
    })

    # y <- reactive(input$covariate, {
    #     # values <-reactiveValues()
    #     req(se)
    #     bd <- batch_design(se, input$covariate)
    #     # values$bd <- bd
    # })

    observe({
        # req(se)
        bd <- batch_design(se, input$covariate)
        output$summaryTable <- renderTable({
            bd
        })
    })
    # output$summaryTable <- renderTable({
    #     # req(se)
    #     bd <- batch_design(se, input$covariate)
    # })



    # output$summaryTable <- renderTable({
    #     ## INGEST USER INPUT
    #     if (!is.null(input$counts) & !is.null(input$md)){
    #         se <- ingest_data(input$counts$datapath, input$md$datapath)
    #     }
    #     else if (!is.null(input$se)){
    #         ### THIS NEEDS TO BE TESTED
    #         se <- SummarizedExperiment(input$se$datapath)
    #     }
    #     else {
    #         se <- NULL
    #     }
    #     ## Display batch design
    #     req(se) ### The following should only run once se is defined
    #     # Get covariate names
    #     cols <- names(colData(se))
    #     covs <- cols[cols != 'Batch']
    #     updateSelectInput(inputId = "covariate", choices = covs)
    #     bd <- batch_design(se, input$covariate)
    #     return(bd)
    #
    #
    # })
