### UPDATING SELECTIONS ###

setupSelections = function(){
  # Experimental design
  updateSelectizeInput(session = session, inputId = "design_batch",
                       choices = names(colData(reactivevalue$se)),
                       selected = NULL)
  updateSelectizeInput(session = session, inputId = "design_covariate",
                       choices = names(colData(reactivevalue$se)),
                       selected = NULL)

  # Normalization
  updateSelectizeInput(session = session, inputId = 'normalization_assay',
                       choices = assayNames((reactivevalue$se)),
                       selected = NULL,
                       options = list(placeholder =
                                      'Please select an option below',
                                      onInitialize = I('function() {
                                                       this.setValue(""); }')))

  # Batch Correction
  updateSelectizeInput(session = session, inputId = "correction_batch",
                       choices = (names(colData(reactivevalue$se))),
                       selected = NULL, options = list(placeholder =
                       'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')))
  updateSelectizeInput(session = session, inputId = 'correction_assay',
                       choices = (assayNames((reactivevalue$se))),
                       selected = NULL, options = list(placeholder =
                       'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')))
  updateSelectizeInput(session = session, inputId = "correction_covariates",
                       choices = (names(colData(reactivevalue$se))),
                       selected = NULL, options = list(placeholder =
                       'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')))

  # Heatmap
  updateSelectizeInput(session = session, inputId = 'heatmap_assay_name',
                       choices = assayNames((reactivevalue$se)),
                       selected = NULL)
  updateSelectInput(session = session, inputId = 'variates_to_display',
                    choices = colnames(colData(reactivevalue$se)),
                    selected = NULL)
  updateNumericInput(session = session, inputId = 'top_n_heatmap',
                     value = 2, min = 2, max = dim(reactivevalue$se)[1])

  # PCA
  updateSelectizeInput(session = session, inputId = 'pca_assays',
                       choices = assayNames((reactivevalue$se)),
                       selected = NULL)
  updateNumericInput(session = session, inputId = 'top_n_PCA',
                     value = 2, min = 2, max = dim(reactivevalue$se)[1])
  updateSelectizeInput(session = session, inputId = 'variates_shape',
                       choices = colnames(colData(reactivevalue$se)),
                       selected = NULL)
  updateSelectizeInput(session = session, inputId = 'variates_color',
                       choices = colnames(colData(reactivevalue$se)),
                       selected = NULL)
  updateSelectizeInput(session, inputId = 'batch',
                       choices = colnames(colData(reactivevalue$se)),
                       selected = NULL)
  updateSelectizeInput(session, inputId = 'group',
                       choices = colnames(colData(reactivevalue$se)),
                       selected = NULL)

  # Variation Analysis
  updateSelectizeInput(session = session, inputId = "variation_assay",
                       choices = names(assays(reactivevalue$se)),
                       selected=NULL)
  updateSelectizeInput(session = session, inputId = "variation_batch",
                       choices = names(colData(reactivevalue$se)),
                       selected=NULL)

  # Differential expression analysis
  updateSelectizeInput(session = session, inputId="DE_assay",
                       choices = names(assays(reactivevalue$se)),
                       selected=NULL)
}


### UPLOAD TAB ###

## Obtain the counts matrix and count table location
observeEvent(input$counts, {
  req(input$counts)
  reactivevalue$counts_location = input$counts$datapath
  counts_table <- read.table(reactivevalue$counts_location, header = TRUE,
                             row.names = 1,
                             sep = get.delim(reactivevalue$counts_location,
                                             n = 10,
                                             delims = c('\t',',')),
                             check.names = FALSE)
  output$counts_header = renderDT((datatable(counts_table)))
  output$counts_dimensions = renderText(paste(dim(counts_table),
                                              c('observations and','samples')))
})

## Obtain the metadata matrix and metadata table location
observeEvent(input$md, {
  req(input$md)
  reactivevalue$metadata_location = input$md$datapath

  reactivevalue$metadata = read.table(reactivevalue$metadata_location,
                                      header = TRUE, row.names = 1,
                                      sep = get.delim(reactivevalue$metadata_location,
                                                      n = 10,
                                                      delims = c('\t',',')))

  metadata_tables <- apply(reactivevalue$metadata, 2, table)
  full_metadata <- c()
  Variable <- c()
  count <- 1
  for (i in metadata_tables){
    full_metadata <- c(full_metadata, i)
    variables <- rep(names(metadata_tables[count]), length(i))
    Variable <- c(Variable, variables)
    count <- count + 1
  }
  Metadata <- names(full_metadata)
  Occurrence <- as.vector(full_metadata)
  full_metadata <- abind(Metadata, Occurrence, Variable, along = 2,
                         make.names = TRUE)

  output$metadata_header = renderDT(datatable(full_metadata))
})

## Obtain count matrix and metadata from the summarized experiment input
observeEvent( input$se, {
  req(input$se)
  reactivevalue$se_location = input$se$datapath
  se = readRDS(input$se$datapath)
  reactivevalue$se = se
  output$se_counts = renderDT(datatable(assays(reactivevalue$se)$counts))
  output$se_dimensions = renderText(paste(dim(reactivevalue$se),
                                          c('observations', 'samples')))

  metadata_table <- as.data.table(colData(reactivevalue$se))
  metadata_list <- apply(metadata_table, 2, table)
  se_metadata <- c()
  Variable <- c()
  count <- 1
  for (i in metadata_list){
    se_metadata <- c(se_metadata,i)
    se_variables <- rep(names(metadata_list[count]), length(i))
    Variable <- c(Variable, se_variables)
    count <- count+1
  }
  Metadata <- names(se_metadata)
  Occurrence <- as.vector(se_metadata)
  se_metadata <- abind(Metadata, Occurrence, Variable, along = 2,
                       make.names = TRUE)

  output$se_meta = renderDT(datatable(se_metadata))
})

## Create summarized experiment object and set up plot options
observeEvent(input$submit, {
  withBusyIndicatorServer("submit", {
    if (!is.null(reactivevalue$counts_location) & !is.null(
      reactivevalue$metadata_location)) {
      se = summarized_experiment(reactivevalue$counts_location,
                                 reactivevalue$metadata_location)
      se <- se[which(rownames(se) !='NA')]
      reactivevalue$se = se
      reactivevalue$metadata = data.frame(colData(reactivevalue$se))
    }
    else if (!is.null(input$se$datapath)){
      se = readRDS(reactivevalue$se_location)
      se = se[rowSums(se@assays@data$counts)>0,]
      reactivevalue$se = se
      reactivevalue$metadata = data.frame(colData(reactivevalue$se))
    }
    output$se_download = renderPrint(reactivevalue$se)
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("se", ".RDS", sep = "")
      },
      content = function(file) {
        saveRDS(reactivevalue$se,file)
      }

    )

    # Display metadata table
    output$metadata = renderDataTable(data.table(data.frame(
      colData(reactivevalue$se)), keep.rownames = TRUE))
    # Add options to input selections
    setupSelections()
  })
})

## Update normalized assay name
observe({
  req(input$normalization_method, input$normalization_assay)
  updateTextInput(session = session, inputId = 'normalized_assay_name',
                  'Name for the normalized Assay',
                  value = paste(input$normalization_assay,
                                input$normalization_method, sep = '_'))
})

## Normalize a selected assay
observeEvent( input$normalize, {
  req(input$normalization_method, input$normalization_assay,
      input$normalized_assay_name)
  withBusyIndicatorServer("normalize", {
    reactivevalue$se = normalize_SE(reactivevalue$se,
                                    input$normalization_method,
                                    input$normalization_assay,
                                    input$normalized_assay_name)
    if (input$log) {
      reactivevalue$se@assays@data[[input$normalized_assay_name]] =
        log(reactivevalue$se@assays@data[[input$normalized_assay_name]])
    }
    setupSelections()
  })
})

## Update batch effect corrected assay name
observe( {
  req(input$correction_assay, input$correction_batch, input$correction_method)
  if (!is.null(input$correction_covariates)){
    if (!is.null(input$group_for_batch)) {
      updateTextInput(session = session,
                      inputId = 'corrected_assay_name',
                      'Name for the corrected assay',
                      value = paste(input$correction_assay,
                                    input$correction_batch,
                                    input$group_for_batch,
                                    input$correction_method,
                                    paste(input$correction_covariates,
                                          collapse = '_'),
                                    sep = '_'))
    }else {
      updateTextInput(session = session, inputId = 'corrected_assay_name',
                      'Name for the corrected assay',
                      value = paste(input$correction_assay,
                                    input$correction_batch,
                                    input$correction_method,
                                    paste(input$correction_covariates,
                                          collapse = '_'),
                                    sep = '_'))
    }
  }else {
    if (!is.null(input$group_for_batch)) {
      updateTextInput(session = session, inputId = 'corrected_assay_name',
                      'Name for the corrected assay',
                      value = paste(input$correction_assay,
                                    input$correction_batch,
                                    input$group_for_batch,
                                    input$correction_method,
                                    sep = '_'))
    }else {
      updateTextInput(session = session,inputId = 'corrected_assay_name',
                      'Name for the corrected assay',
                      value = paste(input$correction_assay,
                                    input$correction_batch,
                                    input$correction_method,
                                    sep = '_'))
    }
  }
})

## Run batch effect correction
observeEvent(input$correct, {
  req(input$correction_assay, input$correction_batch, input$correction_method)
  tryCatch({{
    msg <- sprintf('Start the batch correction process')
    withProgress(message = msg, {
      setProgress(0.5, 'Correcting...')
      reactivevalue$se = batch_correct(reactivevalue$se,
                                       input$correction_method,
                                       input$correction_assay,
                                       input$correction_batch,
                                       group=NULL,
                                       input$correction_covariates,
                                       input$corrected_assay_name)
      setProgress(1, 'Complete!')
    })
  }},
  error = function(error) {
    showNotification('Confounding', type = "error")
    print(error)
  })
  setupSelections()
  showNotification('Batch Correction Completed', type = "message")
})


### EXPERIMENTAL DESIGN TAB ###

## Update experimental design
observeEvent(input$design_covariate, {
  req(input$design_batch, input$design_covariate, reactivevalue$se)
  # Create batch design table
  design <- batch_design(reactivevalue$se, input$design_batch,
                         input$design_covariate)
  output$batch_design <- renderTable(design)
})

## Update confounding design table
observeEvent(input$design_batch, {
  req(input$design_batch, reactivevalue$se)
  conf_stats <- confound_metrics(reactivevalue$se, input$design_batch)
  output$confounding_table <- renderTable(conf_stats, rownames = TRUE)
})


### VARIATION ANALYSIS TAB ###

## Update covariate options to only those that are not confounded with batch
observeEvent(input$variation_batch, {
  req(reactivevalue$se, input$variation_batch)
  covariate_choices <- covariates_not_confounded(reactivevalue$se,
                                                 input$variation_batch)
  updateSelectizeInput(session = session, inputId = "variation_condition",
                       choices = covariate_choices, selected = NULL)
})

## Update variation analysis plot
ev_plot_reactive <- eventReactive(input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay,
      reactivevalue$se)
  # Create boxplot for variation explained by batch, condition, and
  # batch + condition
  tryCatch({
    batchqc_ev_plot <- EV_plotter(reactivevalue$se, input$variation_batch,
                                  input$variation_condition,
                                  input$variation_assay)
    plot(batchqc_ev_plot$EV_boxplot)
  })
})

## Update variation analysis table
ev_table_reactive <- eventReactive(input$variation, {
  req(input$variation_batch, input$variation_condition,
      input$variation_assay, reactivevalue$se)
  # Create boxplot for variation explained by batch, condition,
  # and batch + condition
  tryCatch({
    batchqc_ev_table <- EV_table(reactivevalue$se, input$variation_batch,
                                 input$variation_condition,
                                 input$variation_assay)
    batchqc_ev_table$EV_table
  }, error = function(err) {
    showNotification("At least one covariate is confounded with another!
                     Please choose different covariates.", type = "error")
    print("At least one covariate is confounded with another!
          Please choose different covariates.")
  })
})

## Update pvalue summary table
pvals_summary_reactive <- eventReactive(input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay,
      reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    pval_summary_table <- pval_summary(reactivevalue$se, input$variation_batch,
                                       input$variation_condition,
                                       input$variation_assay)
    pval_summary_table$pval_table
  })
})

## Update batch pvalue boxplot
batch_pvals_reactive <- eventReactive(input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay,
      reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    plot_batch_pvals <- batch_pval_plotter(reactivevalue$se,
                                           input$variation_batch,
                                           input$variation_condition,
                                           input$variation_assay)
    plot_batch_pvals$batch_boxplot
  }, error = function(err) {
  })
})

## Update covariate pvalue boxplot
covariate_pvals_reactive <- eventReactive(input$variation, {
  req(input$variation_batch, input$variation_condition, input$variation_assay,
      reactivevalue$se)
  # Create boxplot for batch pvals
  tryCatch({
    plot_covariate_pvals <- covariate_pval_plotter(reactivevalue$se,
                                                   input$variation_batch,
                                                   input$variation_condition,
                                                   input$variation_assay)
    plot_covariate_pvals$covar_boxplot
  }, error = function(err) {
  })
})

## Display variation and p-value plots and tables
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
      rownames = TRUE,
      striped = TRUE,
      bordered = TRUE,
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



### HEATMAP TAB ###

## Plot heatmap
observeEvent(input$heatmap_plot, {
  req(reactivevalue$se)
  validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1] &&
                  input$top_n_heatmap > 1,
                  "Please select between 2 and the size of your data set
                variates to display"))
  results = heatmap_plotter(reactivevalue$se,
                          input$heatmap_assay_name,
                          input$top_n_heatmap,
                          input$variates_to_display)

  output$correlation_heatmap = renderPlot({
    validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1] &&
                    input$top_n_heatmap > 1,
                    "Please select between 2 and the size of your data set variates to display"))
    results$correlation_heatmap
  }, height = function() {session$clientData$output_correlation_heatmap_width
  })

  output$topn_heatmap = renderPlot({
    validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1],
                  "Value must be less than the dim of your data set"))
    results$topn_heatmap
    }, height = function() {session$clientData$output_topn_heatmap_width
  })

  output$dendrogram = renderPlot({
    validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1],
                  "Value must be less than the dim of your data set"))
    plot(results$dendrogram)
    }, height = function() {session$clientData$output_dendrogram_width
  })

  output$circular_dendrogram = renderPlot({
    validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1],
                  "Value must be less than the dim of your data set"))
    circlize_dendrogram(as.dendrogram(results$dendrogram))
  }, height = function() {session$clientData$output_circular_dendrogram_width
  })
})



### PCA TAB ###

## Plot PCA
observeEvent(input$PCA_plot, {
  req(reactivevalue$se)
  validate(need(input$top_n_PCA <= dim(reactivevalue$se)[1] &&
                  input$top_n_PCA > 1, "Please select a value for the top
                variable features to use that falls within 2 and the size of
                your dataset"))
  assays <- input$pca_assays
  msg <- sprintf('Generating plot for: %s...', paste(assays, collapse=', '))
  withProgress(message=msg, {
    results=PCA_plotter(reactivevalue$se,
                        input$top_n_PCA,
                        input$variates_color,
                        input$variates_shape,
                        assays)
    setProgress(.8, 'Displaying figure...')
    output$PCA = renderPlot({validate(need(input$top_n_PCA <=
                                             dim(reactivevalue$se)[1] &&
                                             input$top_n_PCA > 1,
                                           "Please select a value for the top
                                           variable features to use that falls
                                           within 2 and the size of your
                                           dataset"))
      results$plot})
    output$var_explained = renderTable(
      results$var_explained, rownames = TRUE, digits = 4)
    setProgress(1, 'Complete.')
  })
})

### DIFFERENTIAL EXPRESSION ANALYSIS TAB ###
