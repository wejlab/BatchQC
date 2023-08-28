
tabPanel(
    "Variation Analysis",

    # Application title
    titlePanel("Variation Analysis"),

    sidebarLayout(sidebarPanel(
        h3("Variation Analysis"),
        selectizeInput('variation_assay', 'Select Assay Name', choices = "",
            options = list(placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput('variation_batch', 'Select Batch Variable', choices = "",
            options = list(placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput('variation_condition', 'Select Covariate', choices = "",
            multiple = TRUE,
            options = list(placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }'))),
        withBusyIndicatorUI(actionButton('variation', label = 'Here we go!'))
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Variation Analysis",
                     plotOutput('EV_show_plot'),
                     dataTableOutput('EV_show_table')
            )
        )
        )
    )
)
