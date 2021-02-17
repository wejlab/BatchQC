tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("PCA"
               ),
               tabPanel("Summary"
               ),
               tabPanel("Table"
               ),
               tabPanel("Explained Variation")
             )
           )
         )
)
