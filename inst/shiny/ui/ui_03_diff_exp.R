
tabPanel("Differential Expression",

         # Application title
         titlePanel("Differential Expression"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Expression Plots"
               ),
               tabPanel("Summary"
               ),
               tabPanel("Limma"
               )
             )
           )
         )
)
