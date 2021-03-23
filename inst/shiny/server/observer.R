#### Obtain the Counts matrix and Count table location ####
observeEvent( input$counts, {
  if (is.null(input$counts)) return()
  #reactivevalue$counts = input$counts
  reactivevalue$counts_location=input$counts$datapath
  output$counts_header=renderTable((read.table(reactivevalue$counts_location,header = T,row.names = 1,
                                               sep = get.delim(reactivevalue$counts_location,n = 10,delims = c('\t',',')),check.names = F))[seq(1,10),seq(1,3)])
})

#### Obtain the metadata matrix and metadata table location ####

observeEvent( input$md, {
  if (is.null(input$md)) return()
  reactivevalue$metadata_location=input$md$datapath
  output$metadata_header=renderTable(head(read.table(reactivevalue$metadata_location,header = T,row.names = 1,
                                                     sep = get.delim(reactivevalue$metadata_location,n = 10,delims = c('\t',',')),check.names = F)))

  reactivevalue$metadata = read.table(reactivevalue$metadata_location,header = T,row.names = 1,sep = get.delim(reactivevalue$metadata_location,n = 10,delims = c('\t',',')))
})

#### Obtain Count matrix and metadata from the rds summarizeexperiment ####
observeEvent( input$se, {
  if (is.null(input$se)) return()
  reactivevalue$se_location=input$se$datapath

  se=readRDS(input$se$datapath)
  reactivevalue$se=se
})

### Create summarized experiment object and setup plot options
observeEvent( input$submit, {
  if (!is.null(reactivevalue$counts_location) & !is.null(reactivevalue$metadata_location)) {
    se = summarize_experiment(reactivevalue$counts_location,
                              reactivevalue$metadata_location)
    reactivevalue$se=se
    reactivevalue$metadata=data.frame(colData(reactivevalue$se))
  }
  else if (!is.null(input$se$datapath)){
    se = readRDS(reactivevalue$se_location)
    se = se[rowSums(se@assays@data$counts)>0,]
    reactivevalue$se=se
    reactivevalue$metadata=data.frame(colData(reactivevalue$se))
  }
  # Display metadata table
  output$metadata=renderDataTable(data.table(data.frame(colData(reactivevalue$se)),keep.rownames = T))
  # Add options to input selections
  setupSelections()
})

## Set up plotting options
setupSelections = function(){
  # Experimental design
  updateSelectizeInput(session=session, inputId="design_batch", choices=names(colData(reactivevalue$se)),selected=NULL)
  updateSelectizeInput(session=session, inputId="design_covariate", choices=names(colData(reactivevalue$se)),selected=NULL)
  # Heatmap selections
  updateSelectizeInput(session = session,inputId = 'Normalization_method_heatmap',choices = assayNames((reactivevalue$se)),selected = NULL)
  updateSelectInput(session = session,inputId = 'Variates_to_display',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateNumericInput(session = session,inputId = 'top_n_heatmap',value = 500,min = 0,max = dim(reactivevalue$se)[1])
  # PCA Selections
  updateSelectizeInput(session = session,inputId = 'pca_assays',choices = assayNames((reactivevalue$se)), selected = NULL)
  updateNumericInput(session = session,inputId = 'top_n_PCA',value = 500,min = 0,max = dim(reactivevalue$se)[1])
  updateSelectizeInput(session = session,inputId = 'Variates_shape',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateSelectizeInput(session = session,inputId = 'Variates_color',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateSelectizeInput(session,'batch',choices=colnames(colData(reactivevalue$se)),selected = NULL
  )
  updateSelectizeInput(session,'group',choices=colnames(colData(reactivevalue$se)),selected = NULL
  )

}

### EXPERIMENTAL DESIGN TAB
# Update experimental design
observeEvent(input$design_covariate, {
  req(input$design_batch, input$design_covariate, reactivevalue$se)
  # Create batch design table
  design <- batchDesign(reactivevalue$se, input$design_batch, input$design_covariate)
  output$batch_design <- renderTable(design)
})
# Update confounding design table
observeEvent(input$design_batch, {
  req(input$design_batch, reactivevalue$se)
  conf_stats <- confoundMetrics(reactivevalue$se, input$design_batch)
  output$confoundingTable <- renderTable(conf_stats, rownames = T)
})


observeEvent(input$submit_variables, {
  if (is.null(input$submit_variables)) return()
  if (!is.null(reactivevalue$se)&!is.null(input$group) & !is.null(input$batch)) {
    reactivevalue$batch_Variable_Name = input$batch
    reactivevalue$group_variable_Name = input$group
    variable_overview=data.frame(table(reactivevalue$metadata[,reactivevalue$group_variable_Name],
                                       reactivevalue$metadata[,reactivevalue$batch_Variable_Name]))
    colnames(variable_overview)=c('Biological Group','Batch Group','Number of Sample')
    output$variable_overview=renderDataTable({variable_overview})
  }

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
    }
    else {
      reactivevalue$covariates=colnames(reactivevalue$metadata)[!colnames(reactivevalue$metadata)%in%
                                                                  c(reactivevalue$group_variable_Name,reactivevalue$batch_Variable_Name)]
      updateSelectInput(session = session,inputId = "covariate", choices = reactivevalue$covariates)
    }
  }
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
    assays <- input$pca_assays
    msg <- sprintf('Generating plot for: %s...', paste(assays, collapse=', '))
    withProgress(message=msg, {
    results=PCA_plotter(reactivevalue$se,
                        input$top_n_PCA,
                        input$Variates_color,
                        input$Variates_shape,
                        assays)
    setProgress(.8, 'Displaying figure...')
    output$PCA=renderPlot({(results[['plot']])})
    setProgress(1, 'Complete.')
    })

  }
}
)
