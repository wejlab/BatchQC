# This script will hold the observeEvent function, which monitors and stores input.

#### Obtain the Counts matrix and Count table location ####
observeEvent( input$counts, {
  if (is.null(input$counts)) return()
  reactivevalue$counts = input$counts
  reactivevalue$counts_location=input$counts$datapath
})

#### Obtain the metadata matrix and metadata table location ####

observeEvent( input$md, {
  if (is.null(input$md)) return()
  reactivevalue$metadata_location=input$md$datapath

  reactivevalue$metadata = read.csv(reactivevalue$metadata_location,header = T,row.names = 1)
  updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
  )
  updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
  )

})

#### Obtain Count matrix and metadata from the rds summarizeexperiment ####
observeEvent( input$se, {
  if (is.null(input$se)) return()
  se=readRDS(input$se$datapath)
  reactivevalue$se=se

  updateSelectizeInput(session,'batch',choices=colnames(colData(reactivevalue$se)),selected = NULL
  )
  updateSelectizeInput(session,'group',choices=colnames(colData(reactivevalue$se)),selected = NULL
  )
})

####offer users two tabs to choose batch and biological group from the column names of metadata####
observeEvent( input$group, {
  if (is.null(input$group)) return()
  reactivevalue$group_variable_Name = input$group
})

observeEvent( input$batch, {
  if (is.null(input$batch)) return()
  reactivevalue$batch_Variable_Name = input$batch
})

#### Not satisfy with your pick? Bleh! ####
observeEvent( input$Clear_selction, {
  if (is.null(input$Clear_selction)) return()

  updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
  )
  updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
  )
  reactivevalue$group_variable_Name=NULL
  reactivevalue$batch_Variable_Name=NULL
})



#### Organize the variables, ready the variable names for latter analysis ####

observe({
  if (!is.null(reactivevalue$group_variable_Name)&!is.null(reactivevalue$batch_Variable_Name)){
    if (reactivevalue$group_variable_Name==reactivevalue$batch_Variable_Name) {
      reactivevalue$group_variable_Name=NULL
      reactivevalue$batch_Variable_Name=NULL
      updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
      )
      updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
      )
    }else {
      reactivevalue$covariates=colnames(reactivevalue$metadata)[!colnames(reactivevalue$metadata)%in%
                                                                  c(reactivevalue$group_variable_Name,reactivevalue$batch_Variable_Name)]
      updateSelectInput(session = session,inputId = "covariate", choices = reactivevalue$covariates)
    }
  }
})



#### Create the summarizeexperiment object ####
observe({
  # Look for user file upload
  if (!is.null(reactivevalue$counts_location) & !is.null(reactivevalue$metadata_location)
      & !is.null(reactivevalue$batch_Variable_Name) & !is.null(reactivevalue$group_variable_Name) & is.null(reactivevalue$se)){
    coldata=read.csv(reactivevalue$metadata_location,header = T,row.names = 1,check.names = F)
    counts=read.csv(reactivevalue$counts_location,header = T,row.names = 1,check.names = F)

    # Filter out samples that do not exist, allow redundancies in either input
    reserve_sample=intersect(rownames(coldata),colnames(counts))
    coldata=coldata[reserve_sample,]
    counts=counts[,reserve_sample]
    counts = counts[,match(rownames(coldata),colnames(counts))]

    se = SummarizedExperiment(assay=list(counts=counts
    ), colData=coldata)
    se = ingest_data(se,reactivevalue$group_variable_Name,
                     reactivevalue$batch_Variable_Name)

    reactivevalue$se=se
    updateSelectizeInput(session = session,inputId = 'Normalization_method_heatmap',choices = assayNames((reactivevalue$se)),selected = NULL)
    updateSelectizeInput(session = session,inputId = 'Normalization_method_PCA',choices = assayNames((reactivevalue$se)),selected = NULL)

    updateSelectInput(session = session,inputId = 'Variates_to_display',choices = colnames(colData(reactivevalue$se)),selected = NULL)
    updateSelectInput(session = session,inputId = 'Variates_shape',choices = colnames(colData(reactivevalue$se)),selected = NULL)

    updateSelectInput(session = session,inputId = 'Variates_color',choices = colnames(colData(reactivevalue$se)),selected = NULL)

    updateNumericInput(session = session,inputId = 'top_n_heatmap',value = 500,min = 0,max = dim(reactivevalue$se)[1])
    output$metadata=renderDataTable(data.table(data.frame(colData(reactivevalue$se)),keep.rownames = T))
  }
  else if (!is.null(reactivevalue$se) & !is.null(reactivevalue$batch_Variable_Name) & !is.null(reactivevalue$group_variable_Name) ) {
    reactivevalue$se=ingest_data(reactivevalue$se,reactivevalue$group_variable_Name,
                                 reactivevalue$batch_Variable_Name)
    updateSelectizeInput(session = session,inputId = 'Normalization_method_heatmap',choices = assayNames((reactivevalue$se)),selected = NULL)
    updateSelectizeInput(session = session,inputId = 'Normalization_method_PCA',choices = assayNames((reactivevalue$se)),selected = NULL)
    updateSelectInput(session = session,inputId = 'Variates_to_display',choices = colnames(colData(reactivevalue$se)),selected = NULL)
    updateNumericInput(session = session,inputId = 'top_n_heatmap',value = 500,min = 0,max = dim(reactivevalue$se)[1])
    updateSelectizeInput(session = session,inputId = 'Variates_shape',choices = colnames(colData(reactivevalue$se)),selected = NULL)
    updateSelectizeInput(session = session,inputId = 'Variates_color',choices = colnames(colData(reactivevalue$se)),selected = NULL)
    output$metadata=renderDataTable(data.table(data.frame(colData(reactivevalue$se)),keep.rownames = T))

  }
  #else if (!is.null(input$se)){
  #  se <<- SummarizedExperiment(input$se$datapath)
  #}
  else {
    se <<- NULL
  }
  # Populate drop down menu with covariates

  #reactivevalue$se=se

})



#### Create batch design table when covariate selected ####
observeEvent( input$covariate, {
  req(se)
  output$summaryTable <- renderTable({
    bd <<- batch_design(se, input$covariate)
  })
})



#### Plot Heatmap based on the input ####
observeEvent( input$heatmap_plot, {
  if (!is.null(reactivevalue$se)) {

    results=heatmap_plotter(reactivevalue$se,
                            input$Normalization_method_heatmap,
                            input$top_n_heatmap,
                            reactivevalue$group_variable_Name,
                            input$Variates_to_display)

    output$correlation_heatmap=renderPlot({
      results$correlation_heatmap
    })

    output$topn_heatmap=renderPlot({
      results$topn_heatmap
    })

    output$Dendrogram=renderPlot({
      plot(results$dendrogram)
    })
  }
})



#### Plot PCA based on the input ####

observeEvent( input$PCA_plot, {
  if (!is.null(reactivevalue$se)) {
    require(ggplot2)
    require(plotly)
    results=PCA_plotter(reactivevalue$se,
                        input$Normalization_method_PCA,
                        input$top_n,
                        input$Variates_color,
                        shape=input$Variates_shape)
    output$PCA=renderPlot({(results[['plot']])})
  }
})
