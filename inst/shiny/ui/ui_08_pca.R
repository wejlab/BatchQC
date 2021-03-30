tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             # Options for both plots
             selectizeInput('variates_shape','Choose which variate to show as shape:',choices =c(),multiple = F,selected = NULL),
             numericInput('top_n_PCA','Choose how many top variable features to use:',value = 500,min = 0,max = 500),
             selectizeInput('variates_color','Choose which variates to show as color:',choices =c(),multiple = F,selected = NULL),

             # List of assays to plot from se
             selectizeInput('pca_assays','Assays to plot', choices =c(), multiple = T),

             actionButton('PCA_plot', label = 'Here we go!')

           ),

           # Show a plot of the generated distribution
           mainPanel(
             plotOutput('PCA'),
             h3('Variance Explained'),
             tableOutput('var_explained')
           )
         )
)
