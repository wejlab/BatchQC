tabPanel("Shape",

         # Application title
         titlePanel("Shape"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Batch Variation"
               ),
               tabPanel("Sample-wise Moments"
               ),
               tabPanel("Gene-wise Moments"
               )
             )
           )
         )
)
