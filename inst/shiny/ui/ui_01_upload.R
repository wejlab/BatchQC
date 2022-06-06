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
            h4("Select the type of input you would like to provide:"),
            radioButtons("uploadChoice", "",
                         c("Count File and Metadata File" = "countFile",
                           "Summarized Experiment Object" = "seObject",
                           "Example Data" = "example"
                         )),
            #Only show panel if uploading count and metadata files
            conditionalPanel(condition = "input.uploadChoice == 'countFile'",
                             h4("Upload counts and metadata table"),
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
                                       accept = accepted)),
            conditionalPanel(condition = "input.uploadChoice == 'seObject'",
                             fileInput(
                                 "se",
                                 "Summarized Experiment",
                                 multiple = FALSE,
                                 accept = c(accepted, ".RDS")
                             )),
            conditionalPanel(condition = "input.uploadChoice == 'example'",
                             selectInput("exampleData", "Example Data",
                                         choices = c("", "proteinData"),
                                         selected = ""),),
            withBusyIndicatorUI(actionButton(inputId = 'submit',label = 'Upload'
            ))
        ),

        # Show a table of the inputted data
        mainPanel(
            tabsetPanel(
                tabPanel('Input Summary',
                         h4(strong("Usage")),
                         h5("The table displayed here is a preview of the data selected to be uploaded to BatchQC. Please take a moment to verify that this is the intended data to upload, then select the Upload button to complete uploading the data to BatchQC."),
                         h4(strong("Data Preview")),
                         DTOutput('counts_header'),
                         textOutput('counts_dimensions'),
                         DTOutput('se_counts'),
                         textOutput('se_dimensions'),
                         br()
                         ),
                tabPanel('Full Metadata',
                         h4(strong("Usage")),
                         h5("The table displayed here shows all metadata associated with the uploaded data. Please refer to this table to view sample condition information."),
                         h4(strong("Metadata Table")),
                         DTOutput('metadata_header'),
                         DTOutput('se_meta'),
                         br()
                         ),
                tabPanel('Normalization',
                         h4(strong("Usage")),
                         h5("CPM calculates the counts mapped to a feature relative to the total counts mapped to a sample times one million. CPM may be used to adjust expression count biases introduced by sequencing depth. CPM adjusted values are not recommended for differential expression analysis or within sample comparison."),
                         h5("DESeq calculates the counts mapped to a feature divided by sample-specific size factors. Size factors are determined by the median ratio of gene counts relative to the geometric mean per feature. DESeq may be used to adjust expression count biases introduced by sequencing depth and RNA composition. DESeq adjusted values are not recommended for within sample comparison."),
                         selectizeInput('normalization_method','Choose normalization method',
                                        multiple=FALSE,
                                        choices = c('CPM','DESeq'),
                                        selected = NULL,
                                        options=list(placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('normalization_assay','Choose the assay on which to do normalization',
                                        multiple=FALSE,
                                        choices = c(''),
                                        selected = NULL),
                         textInput(inputId = 'normalized_assay_name','Name for the normalized assay',
                                   value = ''),
                         checkboxInput('log', 'Log transform the results'),
                         withBusyIndicatorUI(actionButton(inputId = 'normalize',
                                                          label = 'Normalize')),
                         br()
                         ),
                tabPanel('Batch Effect Correction',
                         h4(strong("Usage")),
                         h5("Combat-Seq uses a negative binomial regression to model batch effects. It requires untransformed, raw count data to adjust for batch effect. Please select use this with a counts assay"),
                         h5("Combat corrects for Batch effect using a parametric empirical Bayes framework and data should be cleaned and normalized. Therefore, please select a normalized assay to run this on."),
                         selectizeInput('correction_method','Choose correction method',
                                        multiple=FALSE,
                                        choices = c('ComBat-Seq','ComBat'),
                                        selected = NULL,
                                        options=list(placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_assay','Choose the assay on which to do correction',
                                        multiple=FALSE,
                                        choices = c(''),
                                        selected = NULL,
                                        options=list(placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_batch','Select the variable that represents batch',
                                        multiple=FALSE,
                                        choices = c(''),
                                        selected = NULL,
                                        options=list(placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_covariates','Choose the covariates you would like to preserve',
                                        multiple=TRUE,
                                        choices = c(''),
                                        selected = NULL,
                                        options=list(placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }'))),
                         textInput(inputId = 'corrected_assay_name','Name for the corrected assay'),
                         actionButton(inputId = 'correct', label = 'Correct')
                        )

                )
            )
        )
    )

