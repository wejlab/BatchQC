

tabPanel("Variation Analysis",

    # Application title
    titlePanel("Variation and P-Value Analysis"),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
        tabPanel("Variation Analysis",
                 br(),
                 sidebarLayout(
                     sidebarPanel(),
                     mainPanel()
                 )
        ),
        tabPanel("P-Value Analysis"
        ),
        tabPanel("Differential Expression"
        )
    )

)
