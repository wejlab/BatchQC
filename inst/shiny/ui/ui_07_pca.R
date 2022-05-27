tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             # List of assays to plot from se
             selectizeInput('pca_assays','Assays to plot', choices =c(), multiple = TRUE),

             # Options for both plots
             selectizeInput('variates_shape','Choose which variate to show as shape',choices =c(),
                            multiple = FALSE,selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
             selectizeInput('variates_color','Choose which variate to show as color',
                            choices =c(),multiple = FALSE,selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
             numericInput('top_n_PCA','Choose how many top variable features to use',
                          value = 0,min = 0,max = 500),
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
