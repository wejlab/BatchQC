

tabPanel("Differential Expression Analysis",
         
         # Application title
         titlePanel("Differential Expression Analysis"),
         
         sidebarLayout(
           sidebarPanel(
             
             selectizeInput('DE_conditions','Choose analysis conditions',choices =c(),
                            multiple = F,selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
             selectizeInput('DE_method','Choose analysis method',
                            multiple=F,choices = c('t','wilcox','DESeq2'),selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))),
             
             # List of assays to plot from se
             selectizeInput('DE_assay','Choose assay to analyze', choices =c(), multiple = T),
             sliderInput(inputId="slider", label="Select the magnitude of significance value coloring:", min=-300, max=0, value=-150, step = NULL,round = FALSE,ticks = TRUE),
             actionButton('DE_analyze', label = 'Here we go!')
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Results Table",
                        DTOutput('DE_results')
               ),
               tabPanel("Volcano Plot",
                        plotOutput('volcano')
               )
             )
           )
         )
)
