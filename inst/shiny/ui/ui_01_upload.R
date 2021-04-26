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
                "Summarized Experiment",
                multiple = FALSE,
                accept = accepted
            ),
            withBusyIndicatorUI(actionButton(inputId = 'submit',label = 'Upload'))

        ),

        # Show a table of the inputted data
        mainPanel(
            tabsetPanel(
                tabPanel('Input Summary',
                         tableOutput('counts_header'),
                         tableOutput('metadata_header')
                         ),
                tabPanel('Full Metadata',
                         dataTableOutput('metadata')),
                tabPanel('Normalization',
                         selectizeInput('normalization_method','Choose normalization method',multiple=F,choices = c('CPM','DESeq'),selected = NULL),
                         selectizeInput('normalization_assay','Choose the assay on which to do normalization',
                                        multiple=F,choices = c(''),selected = NULL),
                         textInput(inputId = 'normalized_assay_name','Name for the normalized assay',value = ''),
                         checkboxInput('log','Log transform the results'),
                         withBusyIndicatorUI(actionButton(inputId = 'normalize',label = 'Normalize'))
                         ),
                tabPanel('Batch Effect Correction',
                         selectizeInput('correction_method','Choose correction method',
                                        multiple=F,choices = c('ComBat-Seq','ComBat'),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_assay','Choose the assay on which to do correction',
                                        multiple=F,choices = c(''),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_batch','Choose batch variable for correction',
                                        multiple=F,choices = c(''),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         # selectizeInput('group_for_batch','Choose group variables for correction',
                         #                multiple=F,choices = c(''),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         selectizeInput('correction_covariates','Choose covariate variable(s) for correction',
                                        multiple=T,choices = c(''),selected = NULL,options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                         textInput(inputId = 'corrected_assay_name','Name for the corrected assay'),
                         actionButton(inputId = 'correct',label = 'Correct')
                )
            )
        )
    )
)
