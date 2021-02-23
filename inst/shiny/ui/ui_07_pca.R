tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             selectizeInput('Normalization_method_PCA','Which normalization method to use',choices =c(),multiple = F),
             selectizeInput('Variates_shape','Which variates to show as shape',choices =c(),multiple = F,selected = NULL),
             numericInput('top_n_PCA','How many top variable features to use',value = 500,min = 0,max = 500),
             selectizeInput('Variates_color','Which variates to show as color',choices =c(),multiple = F,selected = NULL),
             actionButton('PCA_plot',label = 'Here we go!')
           ),

           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("PCA",
                        plotOutput('PCA',width = 1024,height = 1024)
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
