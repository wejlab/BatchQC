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
                         selectizeInput('Normalization_Method','Choose Normalization Method',multiple=F,choices = c('CPM','DESeq')),
                         selectizeInput('Normalization_Assay','Choose the assay to do normalization',
                                        multiple=F,choices = c('CPM','DESeq')),

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

                         )
            )
        )
    )
)
