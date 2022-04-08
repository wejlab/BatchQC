accepted = c("text/csv",
             "text/comma-separated-values",
             "text/plain",
             ".csv")


tabPanel("Upload Data",
    useShinyjs(),
    tags$style(appCSS),
    tags$div(
        class = "jumbotron",
        tags$div(
            class = "container",
            fluidRow(column(7, h1("BatchQC"))),
            tags$p("Batch Effects Quality Control Software"),
            uiOutput("tab")

        )
    ),
    # Application title
    titlePanel("Upload Data"),

    # Place for uploading data
    sidebarLayout(
        sidebarPanel(
            h3("Upload counts and metadata table"),
            tags$div(tags$p(
                'Metadata file must be a table with headers and sample names.'
            )),
            fileInput(
                "counts",
                "Counts table",
                multiple = FALSE,
                accept = accepted
            ),
            fileInput("md", "Metadata",
                      multiple = FALSE,
                      accept = accepted),
            h4("-OR-"),
            fileInput(
                "se",
                "Summarized Experiment", ".RDS",
                multiple = FALSE,
                accept = accepted
            ),
            withBusyIndicatorUI(actionButton(inputId = 'submit',label = 'Upload'
            ))
        ),

        # Show a table of the inputted data
        mainPanel(
            tabsetPanel(
                tabPanel('Input Summary',
                         h4(strong("Data Preview")),
                         DTOutput('counts_header'),
                         textOutput('counts_dimensions'),
                         DTOutput('se_counts'),
                         textOutput('se_dimensions'),
                         br(),
                         h4(strong("Usage")),
                         h5("The table displayed here is a preview of the data selected to be uploaded to BatchQC. Please take a moment to verify that this is the intended data to upload, then select the Upload button to complete uploading the data to BatchQC."),
                         ),
                tabPanel('Full Metadata',
                         h4(strong("Metadata Table")),
                         DTOutput('metadata_header'),
                         DTOutput('se_meta'),
                         br(),
                         h4(strong("Usage")),
                         h5("The table displayed here shows all metadata associated with the uploaded data. Please refer to this table to view sample condition information.")
                         ),
                tabPanel('Normalization',
                         selectizeInput('normalization_method','Choose normalization method',
                                        multiple=F,choices = c('CPM','DESeq'),selected = NULL,
                                        options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('normalization_assay','Choose the assay on which to do normalization',
                                        multiple=F,choices = c(''),selected = NULL),
                         textInput(inputId = 'normalized_assay_name','Name for the normalized assay',value = ''),
                         checkboxInput('log','Log transform the results'),
                         withBusyIndicatorUI(actionButton(inputId = 'normalize',label = 'Normalize')),
                         br(),
                         h4(strong("Usage")),
                         h5("CPM calculates the counts mapped to a feature relative to the total counts mapped to a sample times one million. CPM may be used to adjust expression count biases introduced by sequencing depth. CPM adjusted values are not recommended for differential expression analysis or within sample comparison."),
                         h5("DESeq calculates the counts mapped to a feature divided by sample-specific size factors. Size factors are determined by the median ratio of gene counts relative to the geometric mean per feature. DESeq may be used to adjust expression count biases introduced by sequencing depth and RNA composition. DESeq adjusted values are not recommended for within sample comparison.")
                         ),
                tabPanel('Batch Effect Correction',
                         selectizeInput('correction_method','Choose correction method',
                                        multiple=F,choices = c('ComBat-Seq','ComBat'),selected = NULL,
                                        options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_assay','Choose the assay on which to do correction',
                                        multiple=F,choices = c(''),selected = NULL,
                                        options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_batch','Choose batch variable for correction',
                                        multiple=F,choices = c(''),selected = NULL,
                                        options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_covariates','Choose covariate variable(s) for correction',
                                        multiple=T,choices = c(''),selected = NULL,
                                        options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         textInput(inputId = 'corrected_assay_name','Name for the corrected assay'),
                         actionButton(inputId = 'correct',label = 'Correct')
                        )

                )
            )
        )
    )

