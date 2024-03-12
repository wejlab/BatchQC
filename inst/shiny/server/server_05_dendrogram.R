### DENDROGRAM TAB ###

## Plot dendrogram
observeEvent(input$dend_plot, {
    req(reactivevalue$se)

    batch_to_display <- input$dend_batch_to_display
    category_to_display <- input$dend_category_to_display

    den_results <- dendrogram_plotter(reactivevalue$se,
        input$dend_assay_name,
        batch_to_display,
        category_to_display)

    output$dendrogram <- renderPlot({
        plot(den_results$dendrogram)
    }, height = function() {
        session$clientData$output_dendrogram_width
    })

    output$circular_dendrogram <- renderPlot({
        plot(den_results$circular_dendrogram)
    }, height = function() {
        session$clientData$output_circular_dendrogram_width
    })
})
