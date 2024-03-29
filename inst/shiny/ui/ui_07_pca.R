tabPanel("PCA Analysis",

         # Application title
         titlePanel("PCA Analysis"),

         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             # List of assays to plot from se
             selectizeInput('pca_assays',
                            'Assays to plot',
                            choices = c(),
                            multiple = TRUE),

             # Options for both plots
             selectizeInput('variates_shape',
                            'Choose which variate to show as shape',
                            choices = c(),
                            multiple = FALSE,
                            selected = NULL,
                            options = list(placeholder =
                                               'Please select an option below',
                                           onInitialize = I('function() { this.setValue(""); }'))),
             selectizeInput('variates_color',
                            'Choose which variate to show as color',
                            choices = c(),
                            multiple = FALSE,
                            selected = NULL,
                            options = list(placeholder = 'Please select an option below',
                                           onInitialize = I('function() { this.setValue(""); }'))),
             numericInput('top_n_PCA',
                          'Choose how many top variable features to use',
                          value = 0,
                          min = 0,
                          max = 500),
             numericInput('firstPC',
                          'Please list the 2 PC\'s you would like to graph
                          \n x axis:',
                          value = 1,
                          min = 1,
                          max = 500),
             numericInput('secondPC',
                          'y axis:',
                          value = 2,
                          min = 1,
                          max = 500),
             checkboxInput("log_option",
                 label = "Select to log data (recommended for sequencing counts
                 data)", value = FALSE),
             actionButton('PCA_plot', label = 'Here we go!')
           ),

           # Show a plot of the generated distribution
           mainPanel(
             plotOutput('PCA'),
             h4('Variance Explained'),
             tableOutput('var_explained')
           )
         )
)
