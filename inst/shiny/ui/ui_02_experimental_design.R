
tabPanel("Experimental Design",

         # Application title
         titlePanel("Experimental Design"),

         # Sidebar with a slider input for number of bins
         tabsetPanel(
           tabPanel(
             "Input",
             selectInput("covariate", "Select Covariate:", choices = ""),
           ),
           tabPanel("Confounding",
                    textOutput("text"),
                    tableOutput("confoundingTable")
           )
         )

)
