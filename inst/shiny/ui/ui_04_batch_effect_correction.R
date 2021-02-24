
tabPanel("Batch Effect Correction",

         # Application title
         titlePanel("Batch Effect Correction"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("ComBat-seq"
               ),
               tabPanel("ComBat"
               ),
               tabPanel("SVA"
               ),
               tabPanel("RUV"
               )
             )
           )
         )
)
