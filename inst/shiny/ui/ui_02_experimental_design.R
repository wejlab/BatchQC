


tabPanel(
  "Experimental Design",

  # Application title
  titlePanel("Experimental Design"),

  sidebarLayout(sidebarPanel(
    h3("Experimental Design"),
    selectizeInput('design_batch', 'Select Batch Variable', choices = "",options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }')
    ))
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Batch Design",
      selectizeInput("design_covariate", "Select Covariate:", choices = "",options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )),
      tableOutput('batch_design')
    ),
    tabPanel(
      "Confounding Statistics",
      tableOutput("confoundingTable")
    )
  )))
)
