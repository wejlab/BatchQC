
tabPanel("Variation Analysis",

         # Application title
         titlePanel("Variation Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
             sidebarPanel(
             ),

             # Show a plot of the generated distribution
             mainPanel(
                 tabsetPanel(
                     tabPanel("Variation Analysis"
                     ),
                     tabPanel("P-Value Analysis"
                     ),
                     tabPanel("Differential Expression"
                     )
                 )
             )
         )
)
