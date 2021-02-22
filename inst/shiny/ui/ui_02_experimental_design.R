
tabPanel("Experimental Design",

         # Application title
         titlePanel("Experimental Design"),

         # Place for uploading data
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a table of the inputted data
           mainPanel(
             tabsetPanel(
               tabPanel("Summary",
                 selectInput("covariate", "Select Covariate:", choices = "")
               ),
               tabPanel("Confounding",
                        textOutput("text"),
                        tableOutput("confoundingTable")
               ),
               tabPanel("ComBat"
                        #selectInput("covariate", "Select Covariate:", choices = ""),
               ),
               tabPanel("ComBat-seq"
                        #selectInput("covariate", "Select Covariate:", choices = ""),
               ),
               tabPanel("SVA"
                        # selectInput("covariate", "Select Covariate:", choices = ""),
               ),
               tabPanel("RUV"
                        #selectInput("covariate", "Select Covariate:", choices = ""),
               )
             )
           )
         )
)
