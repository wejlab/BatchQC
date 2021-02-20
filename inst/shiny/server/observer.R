# This script will hold the observeevent function, which monitor and store input.


# Store the location of the counts data in reactive value
#### Obtain the Counts matrix and Count table location ####
observeEvent( input$counts, {
  if (is.null(input$counts)) return()
  reactivevalue$counts = input$counts
  reactivevalue$counts_location=input$counts$datapath
})

# Store the location of the metadata in reactive value
#### Obtain the metadata matrix and metadata table location ####

observeEvent( input$md, {
  if (is.null(input$md)) return()
  reactivevalue$metadata_location=input$md$datapath

  reactivevalue$metadata = read.csv(reactivevalue$metadata_location,header = T,row.names = 1)
  updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
                       )
  updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
                       )

})
####offer users two tabs to choose batch and biological group from the column names of metadata####
observeEvent( input$group, {
  if (is.null(input$group)) return()

  reactivevalue$group_variable_Name = input$group
})

observeEvent( input$batch, {
  if (is.null(input$batch)) return()

  reactivevalue$batch_Variable_Name = input$batch
})

#### Not satisfy with your pick? Bleh! ####
observeEvent( input$Clear_selction, {
  if (is.null(input$Clear_selction)) return()

  updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
  )
  updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
  )
  reactivevalue$group_variable_Name=NULL
  reactivevalue$batch_Variable_Name=NULL





  })



#### Organize the variables, ready the variable names for latter analysis ####

observe({
  if (!is.null(reactivevalue$group_variable_Name)&!is.null(reactivevalue$batch_Variable_Name)){
    if (reactivevalue$group_variable_Name==reactivevalue$batch_Variable_Name) {
      reactivevalue$group_variable_Name=NULL
      reactivevalue$batch_Variable_Name=NULL
      updateSelectizeInput(session,'batch',choices=colnames(reactivevalue$metadata),selected = NULL
      )
      updateSelectizeInput(session,'group',choices=colnames(reactivevalue$metadata),selected = NULL
      )
}
    else {
      reactivevalue$covariates=colnames(reactivevalue$metadata)[!colnames(reactivevalue$metadata)%in%
                                                                  c(reactivevalue$group_variable_Name,reactivevalue$batch_Variable_Name)]
      updateSelectInput(session = session,inputId = "covariate", choices = reactivevalue$covariates)





      }

  }



})




#### Create the summarizeexperiment object ####
observe({
  # Look for user file upload
  if (!is.null(reactivevalue$counts_location) & !is.null(reactivevalue$metadata_location)
      & !is.null(reactivevalue$batch_Variable_Name) & !is.null(reactivevalue$group_variable_Name)){
    se <<- ingest_data(reactivevalue$counts_location,
                       reactivevalue$metadata_location,
                       reactivevalue$group_variable_Name,
                       reactivevalue$batch_Variable_Name,
                       reactivevalue$covariates)
    reactivevalue$se=se
    updateSelectizeInput(session = session,inputId = 'Normalization_method',choices = assayNames((reactivevalue$se)),selected = NULL)
    updateSelectInput(session = session,inputId = 'Variates_to_display',choices = colnames(colData(reactivevalue$se)),selected = NULL)
    updateNumericInput(session = session,inputId = 'top_n',value = 500,min = 0,max = dim(reactivevalue$se)[1])


  }
  #else if (!is.null(input$se)){
  #  se <<- SummarizedExperiment(input$se$datapath)
  #}
  else {
    se <<- NULL
  }
  # Populate drop down menu with covariates

  #reactivevalue$se=se

})




### Create batch design table when covariate selected
observeEvent(input$covariate, {
  req(se)
  output$summaryTable <- renderTable({
    bd <<- batch_design(se, input$covariate)
  })
})




#### Plot Heatmap based on the input ####


observeEvent( input$heatmap_plot, {
if (!is.null(reactivevalue$se)) {
  require(pheatmap
          )
  data=reactivevalue$se@assays@data[[input$Normalization_method]]
vargenes=apply(data,1,var)
vargenes=vargenes[order(vargenes,decreasing = T)]
vargenes=vargenes[seq(1,input$top_n)]
data=log(data+1)
data=data[names(vargenes),]
coldata=colData(reactivevalue$se)
if (is.null(input$Variates_to_display)) {
cor=cor(data)
coldata=coldata[,c(reactivevalue$group_variable_Name,'Batch')]
output$correlation_heatmap=renderPlot({pheatmap(cor,annotation_col = coldata,annotation_row = coldata,show_colnames = F,show_rownames = F)})
}
else {
  cor=cor(data)
  coldata=coldata[,unique(c(reactivevalue$group_variable_Name,'Batch',input$Variates_to_display))]
  output$correlation_heatmap=renderPlot({pheatmap(cor,annotation_col = coldata,annotation_row = coldata,show_colnames = F,show_rownames = F,)})
}

}
}
)



