### UPLOAD TAB ###

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
observeEvent(input$se, {
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

## Obtain count matrix and count location for example data
observeEvent(input$exampleData, {
    req(input$exampleData)
    # if(input$exampleData == "proteinData"){
    #     reactivevalue$counts_location <- system.file("data/protein_data.rda",
    #                                                  package = "BatchQC")
    #     reactivevalue$metadata_location <- system.file("data/protein_sample_info.rda",
    #                                                    package = "BatchQC")
    # }
    # counts_table <- read.table(reactivevalue$counts_location, header = TRUE,
    #                            row.names = 1,
    #                            sep = get.delim(reactivevalue$counts_location,
    #                                            n = 10,
    #                                            delims = c('\t',',')),
    #                            check.names = FALSE)
    # output$counts_header = renderDT((datatable(counts_table)))
    # output$counts_dimensions = renderText(paste(dim(counts_table),
    #                                             c('observations and','samples')))
    #
    #
    # reactivevalue$metadata = read.table(reactivevalue$metadata_location,
    #                                     header = TRUE, row.names = 1,
    #                                     sep = get.delim(reactivevalue$metadata_location,
    #                                                     n = 10,
    #                                                     delims = c('\t',',')))
    #
    # metadata_tables <- apply(reactivevalue$metadata, 2, table)
    # full_metadata <- c()
    # Variable <- c()
    # count <- 1
    # for (i in metadata_tables){
    #     full_metadata <- c(full_metadata, i)
    #     variables <- rep(names(metadata_tables[count]), length(i))
    #     Variable <- c(Variable, variables)
    #     count <- count + 1
    # }
    # Metadata <- names(full_metadata)
    # Occurrence <- as.vector(full_metadata)
    # full_metadata <- abind(Metadata, Occurrence, Variable, along = 2,
    #                        make.names = TRUE)
    #
    # output$metadata_header = renderDT(datatable(full_metadata))
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
