### HEATMAP TAB ###

## Plot heatmap
observeEvent(input$heatmap_plot, {
    req(reactivevalue$se)
    validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1] &&
                      input$top_n_heatmap > 1,
                  "Please select between 2 and the size of your data set
                variates to display"))
    results <- heatmap_plotter(reactivevalue$se,
                              input$heatmap_assay_name,
                              input$top_n_heatmap,
                              input$variates_to_display)

    output$correlation_heatmap <- renderPlot({
        validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1] &&
                          input$top_n_heatmap > 1,
                      "Please select between 2 and the size of your data set variates to display"))
        results$correlation_heatmap
    }, height = function() {session$clientData$output_correlation_heatmap_width
    })

    output$topn_heatmap <- renderPlot({
        validate(need(input$top_n_heatmap <= dim(reactivevalue$se)[1],
                      "Value must be less than the dim of your data set"))
        results$topn_heatmap
    }, height = function() {session$clientData$output_topn_heatmap_width
    })
})
