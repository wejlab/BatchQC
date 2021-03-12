

tabPanel(
    "Variation Analysis",

    # Application title
    titlePanel("Variation Analysis"),

    sidebarLayout(sidebarPanel(
        h3("Variation Analysis"),
        selectizeInput('variation_assay', 'Select Assay Name', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        selectizeInput('variation_batch', 'Select Batch Variable', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        selectizeInput('variation_condition', 'Select Covariate', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        sliderInput('variation_slider',
                    'Number of genes to include in the table:',
                    min = 1,
                    max = 10,
                    value = 10),
        actionButton('variation',label = 'Here we go!')
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Variation Analysis",
                     plotOutput('EV_plot'),
                     tableOutput('EV_table')
            ),
            tabPanel("P-Value Analysis"
                     # tableOutput("confoundingTable")
            ),
            tabPanel("Differential Expression"
                     # tableOutput("confoundingTable")
            )
        )
        )
    )
)
