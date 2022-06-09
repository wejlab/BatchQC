

tabPanel(
  "Differential Expression Analysis",

  # Application title
  titlePanel("Differential Expression Analysis"),

  sidebarLayout(sidebarPanel(
    h3("Differential Expression Analysis"),
    selectizeInput('DE_assay', 'Select Assay Name', choices = "",options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }')
    ))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Differential Expression Analysis"
      ),
      tabPanel("P-Value Analysis"
      )
    )
  )
  )
)
