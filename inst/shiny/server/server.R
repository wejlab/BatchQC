# Basic structure test:
## reative value: pre-determined name:






shinyServer(function(input, output, session) {
  reactivevalue=reactiveValues(counts_location='',
                               Counts='',
                               metadata_location='',
                               Metadata='',
                               Batch_Variable_Name='',
                               group_variable_Name='')
  source('observer.R',local = T)













}
)
