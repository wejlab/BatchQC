# This script will hold the observeevent function, which monitor and store input.


# Store the location of the counts data in reactive value

observeEvent( input$counts, {
  if (is.null(input$counts)) return()
  reactivevalue$counts_location = input$counts

})

# Store the location of the metadata in reactive value

observeEvent( input$md, {
  if (is.null(input$md)) return()
  reactivevalue$md = input$md

  updateSelectInput(inputId = "group", choices = covs)

})

observe({
  # Look for user file upload
  if (!is.null(input$counts) & !is.null(input$md)){
    se <<- ingest_data(input$counts$datapath, input$md$datapath)
  }
  else if (!is.null(input$se)){
    se <<- SummarizedExperiment(input$se$datapath)
  }
  else {
    se <<- NULL
  }
  # Populate drop down menu with covariates
  req(se)
  covs <- metadata(se)$covariates
  updateSelectInput(inputId = "covariate", choices = covs)
})

