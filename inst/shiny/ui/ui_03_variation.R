

tabPanel(
    "Variation Analysis",

    # Application title
    titlePanel("Variation Analysis"),

    sidebarLayout(sidebarPanel(
        h3("Variation Analysis"),
        selectizeInput('variation_batch', 'Select Batch Variable', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        selectizeInput('variation_condition', 'Select Covariate', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        selectizeInput('variation_assay', 'Select Assay Name', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        )),
        actionButton('variation_plot',label = 'Here we go!')
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Variation Analysis",
                     plotOutput('EV_plot')
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
