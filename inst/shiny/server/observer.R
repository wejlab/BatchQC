# This script will hold the observeevent function, which monitor and store input.


# Store the location of the counts data in reactive value

observeEvent( input$counts, {
  if (is.null(input$counts)) return()
  rv$counts_location = input$counts
  
})

# Store the location of the metadata in reactive value

observeEvent( input$md, {
  if (is.null(input$md)) return()
  rv$md = input$md
  
})



