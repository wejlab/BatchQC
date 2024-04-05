
tabPanel(
  "Experimental Design",

  # Application title
  titlePanel("Experimental Design"),

  sidebarLayout(sidebarPanel(
    h3("Experimental Design"),
    selectizeInput('design_batch',
        'Select Batch Variable',
        choices = "",
        options = list(placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')))
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Batch Design",
      selectizeInput("design_covariate",
          "Select Covariate:",
          choices = "",
          options = list(placeholder = 'Please select an option below',
                        onInitialize = I('function() { this.setValue(""); }'))),
      tableOutput('batch_design'),
      br(),
      h4(strong("Usage")),
      h5("Tables displayed here summarize the batch status of samples in a given condition. Use these tables to preview the experimental design of batches in your sample conditions. These tables will be used to estimate batch effect for sample conditions."),
      h5("First, select the uploaded metadata variable that describes the batch status of your samples in the dropdown menu of Select Batch Variable. Next, select the metadata variable for which condition you would like to view the status of batch in.")
    ),
    tabPanel(
      "Confounding Statistics",
      tableOutput("confounding_table"),
      br(),
      h4(strong("Usage")),
      h5("The table displayed here is a summary of the batch effect estimated to be present in your sample conditions. The metric used for this estimation is Cramer's V. Batch effect values are estimated between 1 and 0. A value closer to 0 indicates lower batch effect and a value closer to 1 indicates high batch effect."),
    )
  )))
)
