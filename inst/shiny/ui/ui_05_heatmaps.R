tabPanel("Heatmaps",

         # Application title
         titlePanel("Heatmaps"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(

             selectizeInput('Normalization_method_heatmap','Which normalization method to use',choices =c(),multiple = F,selected = 'CPM'),
             selectizeInput('Variates_to_display','Which variates to display on heatmap',choices =c(),multiple = T,selected = NULL),
             numericInput('top_n_heatmap','How many top variable features to use',value = 500,min = 0,max = 500),
             actionButton('heatmap_plot',label = 'Here we go!')

           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Sample Correlations",
                        plotOutput('correlation_heatmap',width = 1024,height = 1024)
               ),
               tabPanel("Heatmap",
                        plotOutput('topn_heatmap',width = 1024,height = 1024)

               ),
               tabPanel("Dendrogram",
                        plotOutput('Dendrogram',width = 1024,height = 1024)

               )


             )
           )
         )
)
