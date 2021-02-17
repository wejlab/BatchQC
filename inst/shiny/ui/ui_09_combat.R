tabPanel("ComBat",

         # Application title
         titlePanel("ComBat"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("ComBat Plots"
               ),
               tabPanel("Summary"
               ),
               tabPanel("ComBat Output"
               )
             )
           )
         )
)
