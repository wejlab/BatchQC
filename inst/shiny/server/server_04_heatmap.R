### HEATMAP TAB ###

## Plot heatmap
observeEvent(input$heatmap_plot, {
    req(reactivevalue$se)
    validate(need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                      input$end_n_heatmap > 1,
                  "Please select between 2 and the size of your data set
                variables to display"))
    results <- heatmap_plotter(reactivevalue$se,
                            input$heatmap_assay_name,
                            input$end_n_heatmap,
                            input$variates_to_display,
                            input$log_option)

    output$correlation_heatmap <- renderPlot({
        validate(
            need(input$start_n_heatmap >= 1 &&
                     input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                     input$start_n_heatmap < input$end_n_heatmap,
                 "Please enter start value which is less than end value"),
            need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                     input$end_n_heatmap > input$start_n_heatmap,
                 paste0("Please enter end value between ",
                        input$start_n_heatmap + 1, " and ",
                        dim(reactivevalue$se)[1]))
        )
        results$correlation_heatmap
        }, height = function() {
        session$clientData$output_correlation_heatmap_width
    })

    output$topn_heatmap <- renderPlot({
        validate(
            need(input$start_n_heatmap >= 1 &&
                     input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                     input$start_n_heatmap < input$end_n_heatmap,
                 "Please enter start value which is less than end value"),
            need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                     input$end_n_heatmap > input$start_n_heatmap,
                 paste0("Please enter end value between ",
                        input$start_n_heatmap + 1, " and ",
                        dim(reactivevalue$se)[1]))
        )
        results$topn_heatmap
    }, height = function() {
        session$clientData$output_topn_heatmap_width
    })
})
