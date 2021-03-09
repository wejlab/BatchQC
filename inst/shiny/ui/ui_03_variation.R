

tabPanel(
    "Variation Analysis",

    # Application title
    titlePanel("Variation Analysis"),

    sidebarLayout(sidebarPanel(
        h3("Variation Analysis"),
        selectizeInput('variation_batch', 'Select Batch Variable', choices = "",options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
        ))
    ),
    mainPanel(tabsetPanel(
        tabPanel(
            "Variation Analysis"
            # selectizeInput("design_covariate", "Select Covariate:", choices = "",options = list(
            #     placeholder = 'Please select an option below',
            #     onInitialize = I('function() { this.setValue(""); }')
            # )),
            # tableOutput('batch_design')
        ),
        tabPanel(
            "P-Value Analysis"
            # tableOutput("confoundingTable")
        ),
        tabPanel(
            "Differential Expression"
            # tableOutput("confoundingTable")
        )
    )))
)
