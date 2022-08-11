

tabPanel("Differential Expression Analysis",
         
         # Application title
         titlePanel("Differential Expression Analysis"),
         
         sidebarLayout(
           sidebarPanel(
               
             selectizeInput('DE_assay','Choose assay to analyze', choices = "",options = list(
                 placeholder = 'Please select an option below',
                 onInitialize = I('function() { this.setValue(""); }'))
                            ),
             selectizeInput('DE_batch', 'Choose batch covariate', choices = "",options = list(
                 placeholder = 'Please select an option below',
                 onInitialize = I('function() { this.setValue(""); }'))
             ),
             selectizeInput('DE_conditions','Choose analysis covariates',choices =c(),
                            multiple = T,selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))
             ),
             selectizeInput('DE_method','Choose analysis method',
                            multiple=F,choices = c('DESeq2'),selected = NULL,
                            options=list(placeholder = 'Please select an option below',onInitialize = I('function() { this.setValue(""); }'))
                            ),
             checkboxInput('DE_advanced_options','Advanced options',value = FALSE),
             conditionalPanel(condition = "input.DE_advanced_options == 1",
                              selectizeInput('DE_res_selected', 'Choose analysis results to display', choices = "",options = list(
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))
                              ),
                              sliderInput(inputId="pslider", label="Select the magnitude of significance value coloring:", min=0, max=1, value=0.5,round = FALSE,ticks = TRUE, step = 0.001
                              ),
                              sliderInput(inputId="fcslider", label="Select the magnitude of expression change value coloring:", min=-10, max=10, value=0, step = NULL,round = FALSE,ticks = TRUE
                              )
                              ),
             actionButton('DE_analyze', label = 'Here we go!')
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Results Table",
                        DTOutput('DE_results'),
                        conditionalPanel(condition = "output.DE_results",
                                         downloadButton("downloadDEData", "Download")),
                        textOutput('test')
               ),
               tabPanel("P-Value Analysis",
                        tableOutput('pval_summary'),
                        plotOutput('batch_pval_plot'),
                        plotOutput('covariate_pval_plot')
               ),
               tabPanel("Volcano Plot",
                        plotOutput('volcano')
               )
             )
           )
         )
)
