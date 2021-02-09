#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                 fluidRow(
                   column(7, h1("BatchQC"))
                 ),
                 p("Batch Effects Quality Control Software"),
                 uiOutput("tab")

           )
         ),
    # Application title
    titlePanel("Upload Data"),

    # Place for uploading data
    sidebarLayout(
        sidebarPanel(
        h3("Upload counts and metadata table"),
        tags$div(p('Metadata file must contain "Sample" and "Batch" columns')),
        fileInput("counts", "Counts table",
                  multiple = FALSE,
                  accept = accepted),
        fileInput("md", "Metadata",
                  multiple = FALSE,
                  accept = accepted),
        h3("Or upload a Summarized Experiment"),
        fileInput("se", "Summarized Experiment",
                  multiple = FALSE,
                  accept = accepted)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           # tableOutput("summaryTable")
        )
    )
)
