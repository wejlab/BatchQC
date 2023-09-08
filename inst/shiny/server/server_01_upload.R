### UPLOAD TAB ###

### UPDATING SELECTIONS ###

setupSelections <- function() {
    # Experimental design
    updateSelectizeInput(session = session, inputId = "design_batch",
        choices = names(colData(reactivevalue$se)),
        selected = NULL)
    updateSelectizeInput(session = session, inputId = "design_covariate",
        choices = names(colData(reactivevalue$se)),
        selected = NULL)

    # Normalization
    updateSelectizeInput(session = session, inputId = "normalization_assay",
        choices = assayNames((reactivevalue$se)),
        selected = NULL,
        options = list(placeholder = "Please select an option below",
            onInitialize = I('function() { this.setValue(""); }')))

    # Batch Correction
    updateSelectizeInput(session = session,
        inputId = "correction_batch",
        choices = (names(colData(reactivevalue$se))),
        selected = NULL,
        options = list(placeholder = "Please select an option below",
            onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session = session,
        inputId = "correction_assay",
        choices = (assayNames((reactivevalue$se))),
        selected = NULL,
        options = list(placeholder =
                'Please select an option below',
            onInitialize =
                I('function() { this.setValue(""); }')))
    updateSelectizeInput(session = session,
        inputId = "correction_covariates",
        choices = (names(colData(reactivevalue$se))),
        selected = NULL,
        options = list(placeholder =
                'Please select an option below',
            onInitialize =
                I('function() { this.setValue(""); }')))

    # Heatmap
    updateSelectizeInput(session = session, inputId = 'heatmap_assay_name',
        choices = assayNames((reactivevalue$se)),
        selected = NULL)
    updateSelectInput(session = session, inputId = 'variates_to_display',
        choices = colnames(colData(reactivevalue$se)),
        selected = NULL)
    updateNumericInput(session = session, inputId = 'top_n_heatmap',
        value = 2, min = 2, max = dim(reactivevalue$se)[1])

    # Dendrogram
    updateSelectizeInput(session = session, inputId = 'dend_assay_name',
        choices = assayNames((reactivevalue$se)),
        selected = NULL)
    updateSelectInput(session = session, inputId = 'dend_batch_to_display',
        choices = colnames(colData(reactivevalue$se)),
        selected = NULL)
    updateSelectInput(session = session, inputId = 'dend_category_to_display',
        choices = colnames(colData(reactivevalue$se)),
        selected = NULL)

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
        selected = NULL)
    updateSelectizeInput(session = session, inputId = "variation_batch",
        choices = names(colData(reactivevalue$se)),
        selected = NULL)

    # Differential expression analysis
    updateSelectizeInput(session = session, inputId = "DE_assay",
        choices = names(assays(reactivevalue$se)),
        selected = NULL)
    updateSelectizeInput(session = session, inputId = "DE_batch",
        choices = names(colData(reactivevalue$se)),
        selected = NULL)
}

observeEvent(input$uploadChoice, {
    ##When it changes, set all reactive values to null
})

## Obtain the counts matrix and count table location
observeEvent(input$counts, {
    req(input$counts)
    reactivevalue$counts_location <- input$counts$datapath
    reactivevalue$counts <- read.table(reactivevalue$counts_location,
        header = TRUE,
        row.names = 1,
        sep = get.delim(reactivevalue$counts_location,
            n = 10,
            delims = c('\t', ',')),
        check.names = FALSE)
    output$counts_header <- renderDT((datatable(reactivevalue$counts)))
    output$counts_dimensions <- renderText(paste(dim(reactivevalue$counts),
        c('observations and', 'samples')))
})

## Obtain the metadata matrix and metadata table location
observeEvent(input$md, {
    req(input$md)
    reactivevalue$metadata_location <- input$md$datapath

    reactivevalue$metadata <- read.table(reactivevalue$metadata_location,
        header = TRUE, row.names = 1,
        sep = get.delim(reactivevalue$metadata_location,
            n = 10,
            delims = c('\t', ',')))

    output$metadata_header <- renderDT(datatable(reactivevalue$metadata))
})

## Obtain count matrix and metadata from the summarized experiment input
observeEvent(input$se, {
    req(input$se)
    reactivevalue$se_location <- input$se$datapath
    se <- readRDS(input$se$datapath)
    reactivevalue$se <- se
    output$counts_header <- renderDT(datatable(assays(reactivevalue$se)$counts))
    #output$counts_header <- renderDT((datatable(reactivevalue$counts)))
    output$counts_dimensions <- renderText(paste(dim(reactivevalue$se),
        c('observations and', 'samples')))

    reactivevalue$metadata <- as.data.table(colData(reactivevalue$se))
    output$metadata_header <- renderDT(datatable(reactivevalue$metadata))
})

## Obtain count matrix and count location for example data
observeEvent(input$exampleData, {
    req(input$exampleData)
    if (input$exampleData == "proteinData") {

        data(protein_data)
        reactivevalue$counts <- protein_data
        output$counts_header <- renderDT(datatable(reactivevalue$counts))
        output$counts_dimensions <- renderText(paste(dim(reactivevalue$counts),
            c('observations and', 'samples')))

        data(protein_sample_info)
        reactivevalue$metadata <- protein_sample_info
        output$metadata_header <- renderDT(datatable(reactivevalue$metadata))
    }else if (input$exampleData == "signatureData") {
        data(signature_data)
        reactivevalue$counts <- signature_data
        output$counts_header <- renderDT(datatable(reactivevalue$counts))
        output$counts_dimensions <- renderText(paste(dim(reactivevalue$counts),
            c('observations and', 'samples')))

        data(batch_indicator)
        rownames(batch_indicator) <- batch_indicator$samples
        reactivevalue$metadata <- batch_indicator[2:3]
        output$metadata_header <- renderDT(datatable(reactivevalue$metadata))
    }else if (input$exampleData == "bladderData") {
        bladder_data <- bladder_data_upload()
        reactivevalue$counts <- assays(bladder_data)$counts
        output$counts_header <- renderDT(datatable(reactivevalue$counts))
        output$counts_dimensions <- renderText(paste(dim(reactivevalue$counts),
            c('observations and', 'samples')))

        reactivevalue$metadata <- as.data.frame(colData(bladder_data))
        output$metadata_header <- renderDT(datatable(reactivevalue$metadata))
    }
})

## Create summarized experiment object and set up plot options
observeEvent(input$submit, {
    withBusyIndicatorServer("submit", {
        #need to clear all previous selections
        if (input$uploadChoice == "countFile" &
                !is.null(reactivevalue$counts_location) &
                !is.null(reactivevalue$metadata_location)) {
            se <- summarized_experiment(reactivevalue$counts,
                reactivevalue$metadata)
            se <- se[which(rownames(se) != 'NA')]
        } else if (input$uploadChoice == "seObject" &
                !is.null(input$se$datapath)) {
            se <- readRDS(reactivevalue$se_location)
            se <- se[rowSums(se@assays@data$counts) > 0, ]
        } else if (input$uploadChoice == "example") {
            se <- summarized_experiment(reactivevalue$counts,
                reactivevalue$metadata)
        }

        reactivevalue$se <- se
        reactivevalue$metadata <- data.frame(colData(reactivevalue$se))
        output$se_download <- renderPrint(reactivevalue$se)
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("se", ".RDS", sep = "")
            },
            content = function(file) {
                saveRDS(reactivevalue$se, file)
            }
        )

        # Display metadata table
        output$metadata <- renderDataTable(data.table(data.frame(
            colData(reactivevalue$se)),
            keep.rownames = TRUE))

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
observeEvent(input$normalize, {
    req(input$normalization_method, input$normalization_assay,
        input$normalized_assay_name)
    withBusyIndicatorServer("normalize", {
        reactivevalue$se <- normalize_SE(reactivevalue$se,
            input$normalization_method,
            input$normalization_assay,
            input$normalized_assay_name)
        if (input$log) {
            reactivevalue$se@assays@data[[input$normalized_assay_name]] <-
                log(reactivevalue$se@assays@data[[input$normalized_assay_name]])
        }
        setupSelections()
    })
})

## Update batch effect corrected assay name
observe( {
    req(input$correction_assay, input$correction_batch, input$correction_method)
    if (!is.null(input$correction_covariates)) {
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
            updateTextInput(session = session, inputId = 'corrected_assay_name',
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
            reactivevalue$se <- batch_correct(reactivevalue$se,
                input$correction_method,
                input$correction_assay,
                input$correction_batch,
                group = NULL,
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
