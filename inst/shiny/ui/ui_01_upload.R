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
            actionButton(inputId = 'submit',label = 'Submit')

        ),

        # Show a table of the inputted data
        mainPanel(
            tabsetPanel(
                tabPanel('Preview the first 10 lines of the input.',
                         tableOutput('counts_header'),
                         tableOutput('metadata_header')


                         ),
                tabPanel('Normalization',
                         actionButton(inputId = 'DESEQ_normalization',label = 'DESEQ normalization'),
                         actionButton(inputId = 'CPM_Normalization',label = 'CPM normalization')

                ),
                tabPanel('Full Metadata',
                         dataTableOutput('metadata')),
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
                         actionButton(inputId = 'submit_variables',label = 'Submit'),
                         dataTableOutput('variable_overview')

                         ),
                tabPanel(
                    "Input",
                    selectInput("covariate", "Select Covariate:", choices = ""),
                ),
                tabPanel("Confounding",
                         textOutput("text"),
                         tableOutput("confoundingTable")
                )
            )
        )
    )
)
