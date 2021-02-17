tabPanel("Heatmaps",

         # Application title
         titlePanel("Heatmaps"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Heatmap"
               ),
               tabPanel("Sample Correlations"
               )
             )
           )
         )
)
