tabPanel("Heatmaps",

         # Application title
         titlePanel("Heatmaps"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(

             selectizeInput('Normalization_method','Which normalization method to use',choices =c(),multiple = F,selected = 'CPM'),
             selectizeInput('Variates_to_display','Which variates to display on heatmap',choices =c(),multiple = T,selected = NULL),
             numericInput('top_n','How many top variable features to use',value = 500,min = 0,max = 500),
             actionButton('heatmap_plot',label = '走你！')

           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Sample Correlations",
                        plotOutput('correlation_heatmap',width = 1024,height = 1024)
               ),
               tabPanel("Heatmap"
               )

             )
           )
         )
)
