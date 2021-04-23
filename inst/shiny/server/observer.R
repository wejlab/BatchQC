### Obtain the counts matrix and count table location ###
observeEvent( input$counts, {
  req(input$counts)
  reactivevalue$counts_location=input$counts$datapath
  output$counts_header=renderTable((read.table(reactivevalue$counts_location,header = T,row.names = 1,
                                               sep = get.delim(reactivevalue$counts_location,n = 10,delims = c('\t',',')),check.names = F))[seq(1,10),seq(1,3)])
})

### Obtain the metadata matrix and metadata table location ###
observeEvent( input$md, {
  req(input$md)
  reactivevalue$metadata_location=input$md$datapath
  output$metadata_header=renderTable(head(read.table(reactivevalue$metadata_location,header = T,row.names = 1,
                                                     sep = get.delim(reactivevalue$metadata_location,n = 10,delims = c('\t',',')),check.names = F)))

  reactivevalue$metadata = read.table(reactivevalue$metadata_location,header = T,row.names = 1,sep = get.delim(reactivevalue$metadata_location,n = 10,delims = c('\t',',')))
})

### Obtain Count matrix and metadata from the rds summarized experiment ###
observeEvent( input$se, {
  req(input$se)
  reactivevalue$se_location=input$se$datapath
  se=readRDS(input$se$datapath)
  reactivevalue$se=se
})

### Create summarized experiment object and set up plot options ###
observeEvent( input$submit, {

  withBusyIndicatorServer("submit", {
    if (!is.null(reactivevalue$counts_location) & !is.null(reactivevalue$metadata_location)) {
      se = summarized_experiment(reactivevalue$counts_location,
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

})
### Normalization ###
observe( if (!is.null(input$Normalization_Method)&
             !is.null(input$Normalization_Assay)) {
  updateTextInput(session = session,inputId = 'Normalization_Results_Name','Name for the normalized Assay',value = paste(input$Normalization_Assay,
                                                                                                       input$Normalization_Method,
                                                                                                       sep = '_'))
})

observeEvent( input$Normalize, if (!is.null(input$Normalization_Method)&
             !is.null(input$Normalization_Assay)
             &!is.null(input$Normalization_Results_Name)) {
  msg <- sprintf('Normalizing')

  withProgress(message=msg, {

  reactivevalue$se=NormalizateSE(reactivevalue$se,
                                 input$Normalization_Method,
                                 input$Normalization_Assay,
                                 input$Normalization_Results_Name)
  if (input$Log) {
    reactivevalue$se@assays@data[[input$Normalization_Results_Name]]=log(reactivevalue$se@assays@data[[input$Normalization_Results_Name]])
  }
  setupSelections()
  setProgress(1, 'Completed')

  })
})

### Batch Correction ###
observe( if (!is.null(input$Correct_Assay)&
             !is.null(input$Batch_for_Batch)&
             !is.null(input$Correct_Method)) {
  if (!is.null(input$covariates_for_Batch)){
    if (!is.null(input$Group_for_Batch)) {
      updateTextInput(session = session,inputId = 'Batch_Results_Name','Name for the corrected Assay',value = paste(input$Correct_Assay,
                                                                                                                    input$Batch_for_Batch,input$Group_for_Batch,input$Correct_Method,paste(input$covariates_for_Batch,collapse = '_'),
                                                                                                                    sep = '_'))
    }
    else {
      updateTextInput(session = session,inputId = 'Batch_Results_Name','Name for the corrected Assay',value = paste(input$Correct_Assay,
                                                                                                                    input$Batch_for_Batch,input$Correct_Method,paste(input$covariates_for_Batch,collapse = '_'),
                                                                                                                    sep = '_'))
    }

  }
  else {
    if (!is.null(input$Group_for_Batch)) {

    updateTextInput(session = session,inputId = 'Batch_Results_Name','Name for the corrected Assay',value = paste(input$Correct_Assay,
                                                                                                                  input$Batch_for_Batch,input$Group_for_Batch,input$Correct_Method,
                                                                                                                  sep = '_'))
    }
    else {
      updateTextInput(session = session,inputId = 'Batch_Results_Name','Name for the corrected Assay',value = paste(input$Correct_Assay,
                                                                                                                    input$Batch_for_Batch,input$Correct_Method,
                                                                                                                    sep = '_'))
    }
  }
})



observeEvent( input$Correct, if (!is.null(input$Correct_Assay)&
                                   !is.null(input$Batch_for_Batch)
                                   &!is.null(input$Correct_Method)) {
  tryCatch({{
    msg <- sprintf('Start the batch correction process')

    withProgress(message=msg, {
      setProgress(0.5, 'Correcting...')

    reactivevalue$se=BatchCorrect(reactivevalue$se,
                                input$Correct_Method,
                                input$Correct_Assay,input$Batch_for_Batch,#input$Group_for_Batch,
                                input$covariates_for_Batch,input$Batch_Results_Name)
    setProgress(1, 'Complete!')

    })
    }},
           error = function(error) {
             showNotification('Confounding', type = "error")
             print(error)
           }
           )
  setupSelections()
  showNotification('Batch Correction Completed', type = "message")
})

### Set up plotting options ###
setupSelections = function(){
  # Experimental design
  updateSelectizeInput(session=session, inputId="design_batch", choices=names(colData(reactivevalue$se)),selected=NULL)
  updateSelectizeInput(session=session, inputId="design_covariate", choices=names(colData(reactivevalue$se)),selected=NULL)

  # Normalization
  updateSelectizeInput(session = session,inputId = 'Normalization_Assay',choices = assayNames((reactivevalue$se)),selected = NULL)
  # Batch Correction
  updateSelectizeInput(session=session, inputId="Batch_for_Batch", choices=(names(colData(reactivevalue$se))),selected=NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }')))
  #updateSelectizeInput(session=session, inputId="Group_for_Batch", choices=(names(colData(reactivevalue$se))),selected=NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }')))
  updateSelectizeInput(session = session,inputId = 'Correct_Assay',choices = (assayNames((reactivevalue$se))),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }')))
  updateSelectizeInput(session=session, inputId="covariates_for_Batch", choices=(names(colData(reactivevalue$se))),selected=NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }')))

  # Heatmap selections
  updateSelectizeInput(session = session,inputId = 'normalization_method_heatmap',choices = assayNames((reactivevalue$se)),selected = NULL)
  updateSelectInput(session = session,inputId = 'variates_to_display',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateNumericInput(session = session,inputId = 'top_n_heatmap',value = 500,min = 0,max = dim(reactivevalue$se)[1])
  # PCA Selections
  updateSelectizeInput(session = session,inputId = 'pca_assays',choices = assayNames((reactivevalue$se)), selected = NULL)
  updateNumericInput(session = session,inputId = 'top_n_PCA',value = 500,min = 0,max = dim(reactivevalue$se)[1])
  updateSelectizeInput(session = session,inputId = 'variates_shape',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateSelectizeInput(session = session,inputId = 'variates_color',choices = colnames(colData(reactivevalue$se)),selected = NULL)
  updateSelectizeInput(session,'batch',choices=colnames(colData(reactivevalue$se)),selected = NULL)
  updateSelectizeInput(session,'group',choices=colnames(colData(reactivevalue$se)),selected = NULL)
  # Variation Analysis selections
  updateSelectizeInput(session=session, inputId="variation_assay", choices=names(assays(reactivevalue$se)),selected=NULL)
  updateSelectizeInput(session=session, inputId="variation_batch", choices=names(colData(reactivevalue$se)),selected=NULL)

  # Differential expression analysis Assay
  updateSelectizeInput(session=session, inputId="DE_assay", choices=names(assays(reactivevalue$se)),selected=NULL)


}


### EXPERIMENTAL DESIGN TAB ###
# Update experimental design
observeEvent(input$design_covariate, {
  req(input$design_batch, input$design_covariate, reactivevalue$se)
  # Create batch design table
  design <- batch_design(reactivevalue$se, input$design_batch, input$design_covariate)
  output$batch_design <- renderTable(design)
})
# Update confounding design table
observeEvent(input$design_batch, {
  req(input$design_batch, reactivevalue$se)
  conf_stats <- confound_metrics(reactivevalue$se, input$design_batch)
  output$confounding_table <- renderTable(conf_stats, rownames = T)
})


### VARIATION ANALYSIS TAB ###
# Update covariate options to only those that are not confounded with batch
observeEvent(input$variation_batch, {
  req(reactivevalue$se, input$variation_batch)
  covariate_choices <- covariates_not_confounded(reactivevalue$se,input$variation_batch)
  updateSelectizeInput(session=session, inputId="variation_condition", choices=covariate_choices,selected=NULL)
})
# Update variation analysis plot
ev_plot_reactive <- eventReactive( input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay, reactivevalue$se)
  # Create boxplot for variation explained by batch, condition, and batch + condition
  tryCatch({
    batchqc_ev_plot <- EV_plotter(reactivevalue$se, input$variation_batch, input$variation_condition, input$variation_assay)
    plot(batchqc_ev_plot$EV_boxplot)
  })
})
# Update variation analysis table
ev_table_reactive <- eventReactive( input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay, reactivevalue$se)
  # Create boxplot for variation explained by batch, condition, and batch + condition
  tryCatch({
    batchqc_ev_table <- EV_table(reactivevalue$se, input$variation_batch, input$variation_condition, input$variation_assay)
    batchqc_ev_table$EV_table
  }, error = function(err) {
    showNotification("At least one covariate is confounded with another! Please choose different covariates.", type = "error")
    print("At least one covariate is confounded with another! Please choose different covariates.")
  })
})
# Update pvalue summary table
pvals_summary_reactive <- eventReactive( input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay, reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    pval_summary_table <- pval_summary(reactivevalue$se, input$variation_batch, input$variation_condition, input$variation_assay)
    pval_summary_table$pval_table
  })
})
# Update batch pvalue boxplot
batch_pvals_reactive <- eventReactive( input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay, reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    plot_batch_pvals <- batch_pval_plotter(reactivevalue$se, input$variation_batch, input$variation_condition, input$variation_assay)
    plot_batch_pvals$batch_boxplot
  }, error = function(err) {
  })
})
# Update covariate pvalue boxplot
covariate_pvals_reactive <- eventReactive( input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay, reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    plot_covariate_pvals <- covariate_pval_plotter(reactivevalue$se, input$variation_batch, input$variation_condition, input$variation_assay)
    plot_covariate_pvals$covar_boxplot
  }, error = function(err) {
  })
})
# Display variation and p-value plots and tables
observeEvent(input$variation, {
  withBusyIndicatorServer("variation", {
    output$EV_show_plot <- renderPlot({
      ev_plot_reactive()
    })

    output$EV_show_table <- renderDataTable({
      ev_table_reactive()
    })

    output$pval_summary <- renderTable({
      pvals_summary_reactive()},
      rownames= T,
      striped = T,
      bordered = T,
      caption = "<b> <span style='color:#000000'> P-Value Summary Table </b>",
      caption.placement = getOption("xtable.caption.placement","top"),
    )

    output$batch_pval_plot <- renderPlot({
      batch_pvals_reactive()
    })

    output$covariate_pval_plot <- renderPlot({
      covariate_pvals_reactive()
    })
  })
})


#### Setting global batch and covariate variables ####
observeEvent(input$submit_variables, {
  req(input$submit_variables, reactivevalue$se, input$batch)
    reactivevalue$batch_Variable_Name = input$batch
    reactivevalue$group_variable_Name = input$group
    variable_overview=data.frame(table(reactivevalue$metadata[,reactivevalue$group_variable_Name],
                                       reactivevalue$metadata[,reactivevalue$batch_Variable_Name]))
    colnames(variable_overview)=c('Biological Group','Batch Group','Number of Samples')
    output$variable_overview=renderDataTable({variable_overview})
})


#### Organize the variables, ready the variable names for later analysis ####
observe({
  req(reactivevalue$group_variable_name, reactivevalue$batch_variable_name)
  if (reactivevalue$group_variable_name==reactivevalue$batch_variable_name) {
    reactivevalue$group_variable_name=NULL
    reactivevalue$batch_variable_name=NULL
    updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL)
    updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL)
  }else {
    reactivevalue$covariates=colnames(reactivevalue$metadata)[!colnames(reactivevalue$metadata)%in%
                                                                c(reactivevalue$group_variable_name,reactivevalue$batch_variable_name)]
    updateSelectInput(session = session,inputId = "covariate", choices = reactivevalue$covariates)
  }
})


#### Plot heatmap  ####
observeEvent( input$heatmap_plot, {
  req(reactivevalue$se)
  results=heatmap_plotter(reactivevalue$se,
                          input$normalization_method_heatmap,
                          input$top_n_heatmap,
                          reactivevalue$group_variable_name,
                          input$variates_to_display)

  output$correlation_heatmap=renderPlot({
    results$correlation_heatmap
  }, height = function() {session$clientData$output_correlation_heatmap_width})
  output$topn_heatmap=renderPlot({
    results$topn_heatmap
  }, height = function() {session$clientData$output_topn_heatmap_width})

  output$dendrogram=renderPlot({
    plot(results$dendrogram)
  }, height = function() {session$clientData$output_dendrogram_width})
})


#### Plot PCA ####
observeEvent( input$PCA_plot, {
  req(reactivevalue$se)
  assays <- input$pca_assays
  msg <- sprintf('Generating plot for: %s...', paste(assays, collapse=', '))
  withProgress(message=msg, {
    results=PCA_plotter(reactivevalue$se,
                        input$top_n_PCA,
                        input$variates_color,
                        input$variates_shape,
                        assays)
    setProgress(.8, 'Displaying figure...')
    output$PCA=renderPlot(results$plot)
    output$var_explained=renderTable(results$var_explained, rownames=T, digits=4)
    setProgress(1, 'Complete.')
  })
})
