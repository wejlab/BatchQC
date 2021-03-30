tabPanel("Heatmaps",

         # Application title
         titlePanel("Heatmaps"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(

             selectizeInput('normalization_method_heatmap','Choose normalization method:',choices =c(),multiple = F,selected = 'CPM'),
             selectizeInput('variates_to_display','Choose variates to display on heatmap:',choices =c(),multiple = T,selected = NULL),
             numericInput('top_n_heatmap','Choose how many top variable features to use:',value = 500,min = 0,max = 500),
             actionButton('heatmap_plot',label = 'Here we go!')

           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Sample Correlations",
                        plotOutput('correlation_heatmap')
               ),
               tabPanel("Heatmap",
                        plotOutput('topn_heatmap')
               ),
               tabPanel("Dendrogram",
                        plotOutput('dendrogram')
               )


             )
           )
         )
)
