tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             # Options for both plots
             selectizeInput('Variates_shape','Which variates to show as shape',choices =c(),multiple = F,selected = NULL),
             numericInput('top_n_PCA','How many top variable features to use',value = 500,min = 0,max = 500),
             selectizeInput('Variates_color','Which variates to show as color',choices =c(),multiple = F,selected = NULL),

             # Left plot data
             selectizeInput('pca_data_left','Left Plot Data',choices =c(),multiple = F),

             # Right plot data
             selectizeInput('pca_data_right','Right Plot Data',choices =c(),multiple = F, selected=NULL),

             actionButton('PCA_plot',label = 'Here we go!')
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("PCA",
                        plotOutput('PCA',width = 600,height = 600)
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
