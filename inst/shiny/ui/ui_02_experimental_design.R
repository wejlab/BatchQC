


tabPanel(
  "Experimental Design",

  # Application title
  titlePanel("Experimental Design"),

  sidebarLayout(sidebarPanel(
    h3("Experimental Design"),
    selectInput('design_batch', 'Select Batch Variable', choices = "")
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Batch Design",

      selectInput("design_covariate", "Select Covariate:", choices = ""),
    ),
    tabPanel(
      "Confounding Statistics",
      textOutput("text"),
      tableOutput("confoundingTable")
    )
  )))
)
