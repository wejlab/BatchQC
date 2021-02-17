tabPanel("SVA",

         # Application title
         titlePanel("SVA"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Summary"
               ),
               tabPanel("SVA Output"
               )
             )
           )
         )
)
