

tabPanel(
  "Differential expression Analysis",

  # Application title
  titlePanel("Differential expression Analysis"),

  sidebarLayout(sidebarPanel(
    h3("Differential expression Analysis"),
    selectizeInput('DE_assay', 'Select Assay Name', choices = "",options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }')
    ))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Differential expression Analysis"
      ),
      tabPanel("P-Value Analysis"
      )
    )
  )
  )
)
