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
                'Metadata file must contain "Sample" and "Batch" columns'
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
            h3("Or upload a Summarized Experiment"),
            fileInput(
                "se",
                "Summarized Experiment",
                multiple = FALSE,
                accept = accepted
            )
        ),

        # Show a table of the inputted data
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Input",
                    selectInput("covariate", "Select Covariate:", choices = ""),
                    tableOutput("summaryTable"),
                ),
                tabPanel("Confounding",
                         textOutput("text")
                         # tableOutput("confoundingTable")))
                )
            )
        )
    )
)
