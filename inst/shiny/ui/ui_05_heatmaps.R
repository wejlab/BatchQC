tabPanel("Heatmaps",

    # Application title
    titlePanel("Heatmaps"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput('heatmap_assay_name',
                'Choose assay to display',
                choices = c(),
                multiple = FALSE,
                selected = NULL,
                options = list(placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }'))),
            selectizeInput('variates_to_display',
                'Choose variable(s) to display on heatmap',
                choices = c(),
                multiple = TRUE,
                selected = NULL),
            numericInput('start_n_heatmap',
                'Choose start value for features (e.g., genes) to be used',
                value = 1,
                min = 1,
                max = 500),
            numericInput('end_n_heatmap',
                'Choose max value for features (e.g., genes) to be used
                (we recommend 500 or less for large datasets)',
                value = 500,
                min = 2),
            checkboxInput("log_option",
                label = "Select to log data (recommended for sequencing counts
                 data)", value = FALSE),
            actionButton('heatmap_plot', label = 'Here we go!')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Sample Correlations",
                    plotOutput('correlation_heatmap')
                ),
                tabPanel("Heatmap",
                    plotOutput('topn_heatmap')
                )
            )
        )
    )
)
