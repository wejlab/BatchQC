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
            #selectizeInput('group','Biological setting Column',choices =c(),multiple = F,selected = NULL,
            #               options = list(
            #                   placeholder = 'Please select an option below',
            #                   onInitialize = I('function() { this.setValue(""); }')
            #               )),
            #selectizeInput('batch','Batch Variable Column',choices =c(),multiple = F,selected = NULL,
            #               options = list(
            #                   placeholder = 'Please select an option below',
            #                   onInitialize = I('function() { this.setValue(""); }')
            #               )),
            actionButton(inputId = 'submit',label = 'Upload')

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
                         selectizeInput('Normalization_Method','Choose Normalization Method',multiple=F,choices = c('CPM','DESeq'),selected = NULL),
                         selectizeInput('Normalization_Assay','Choose the assay to do normalization',
                                        multiple=F,choices = c(''),selected = NULL),
                         textInput(inputId = 'Normalization_Results_Name','Name for the normalized Assay',value = ''),
                         actionButton(inputId = 'Normalize',label = 'Normalize')
                         ),

                tabPanel('Setting Variables',
                         selectizeInput('group','Biological setting Column',choices =c(),multiple = F,selected = NULL,
                                        options = list(
                                            placeholder = 'Please select an option below',
                                            onInitialize = I('function() { this.setValue(""); }')
                                        )),
                         selectizeInput('batch','Batch Variable Column',choices =c(),multiple = F,selected = NULL,
                                        options = list(
                                            placeholder = 'Please select an option below',
                                            onInitialize = I('function() { this.setValue(""); }')
                                        )),
                         actionButton(inputId = 'submit_variables',label = 'Submit')

                         ),
                tabPanel('Batch effect Correction',
                         selectizeInput('Correct_Method','Choose correct method',multiple=F,choices = c('ComBat-Seq','ComBat'),selected = NULL),
                         selectizeInput('Correct_Assay','Choose the assay to do correction',multiple=F,choices = c(''),selected = NULL),
                         selectizeInput('Batch_for_Batch','Choose batch variables for correction',multiple=F,choices = c(''),selected = NULL),
                         selectizeInput('Group_for_Batch','Choose group variables for correction',multiple=F,choices = c(''),selected = NULL),

                         selectizeInput('covariates_for_Batch','Choose Covariate variables for correction',multiple=T,choices = c(''),selected = NULL),

                         textInput(inputId = 'Batch_Results_Name','Name for the corrected Assay'),
                         actionButton(inputId = 'Correct',label = 'Correct')
                )
            )
        )
    )
)
