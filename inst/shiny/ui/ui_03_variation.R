
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
            tabPanel("Explained Variation - Individual Variable",
                h5("The boxplot and p-value table display the individual, or raw variation, explained by each variable."),
                     plotOutput('EV_show_plot'),
                     dataTableOutput('EV_show_table')
            ),
            tabPanel("Explained Variation - Residual",
                h5("The boxplot and p-value table display the type 2 variation, or residual varaition."),
                plotOutput('EV_residual_show_plot'),
                dataTableOutput('EV_residual_show_table'))
        )
        )
    )
)
