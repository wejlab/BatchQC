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
