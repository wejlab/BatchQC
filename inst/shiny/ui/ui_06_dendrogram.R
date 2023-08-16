tabPanel("Dendrograms",

         # Application title
         titlePanel("Dendrograms"),

         sidebarLayout(
             sidebarPanel(
                 selectizeInput('dend_assay_name','Choose assay to display',choices =c(),multiple = FALSE,selected = NULL,
                                options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput('dend_variates_to_display','Choose variate(s) to display on dendogram',
                                choices =c(),multiple = TRUE,selected = NULL),
                 actionButton('dend_plot',label = 'Here we go!')
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
