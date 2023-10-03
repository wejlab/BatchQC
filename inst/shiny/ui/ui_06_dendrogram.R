tabPanel("Dendrograms",

         # Application title
         titlePanel("Dendrograms"),

         sidebarLayout(
             sidebarPanel(
                 selectizeInput('dend_assay_name',
                     'Choose assay to display',
                     choices = c(),
                     multiple = FALSE,
                     selected = NULL,
                     options = list(placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput('dend_batch_to_display',
                     'Choose leaf variable to display on dendogram',
                     choices = c(),
                     multiple = FALSE,
                     selected = NULL,
                     options = list(placeholder = 'Please select an option below',
                                    onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput('dend_category_to_display',
                     'Choose stem variable to display on dendogram',
                     choices = c(),
                     multiple = FALSE,
                     selected = NULL),
                 checkboxInput(inputId = "switch",
                               label = "Switch the choices",
                               value = FALSE
                               ),
                 actionButton('dend_plot', label = 'Here we go!')
             ),

             # Show a plot of the dendrograms
             mainPanel(
                 tabsetPanel(
                     tabPanel("Dendrogram",
                              plotOutput('dendrogram')
                     ),
                     tabPanel("Circular Dendrogram",
                              plotOutput('circular_dendrogram')
                     )
                 )
             )
         )
)
