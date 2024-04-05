### HEATMAP TAB ###

## Plot heatmap
observeEvent(input$heatmap_plot, {
    req(reactivevalue$se)

    validate(need(input$start_n_heatmap >= 1 &&
            input$start_n_heatmap <= (dim(reactivevalue$se)[1] - 1) &&
            input$start_n_heatmap < input$end_n_heatmap,
        paste0("Please enter a start value which is between 1 and ",
            dim(reactivevalue$se)[1],
            " and less than your desired end value (", input$end_n_heatmap,
            ")")),
        need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                input$end_n_heatmap > input$start_n_heatmap,
            paste0("Please select an end value greater than your start value (",
                input$start_n_heatmap, ") and less than ",
                dim(reactivevalue$se)[1])),
    )

    results <- heatmap_plotter(reactivevalue$se,
                            input$heatmap_assay_name,
                            input$end_n_heatmap,
                            input$variates_to_display,
                            input$log_option)

    output$correlation_heatmap <- renderPlot({
        validate(need(input$start_n_heatmap >= 1 &&
                input$start_n_heatmap <= (dim(reactivevalue$se)[1] - 1) &&
                input$start_n_heatmap < input$end_n_heatmap,
            paste0("Please enter a start value which is between 1 and ",
                dim(reactivevalue$se)[1],
                " and less than your desired end value (", input$end_n_heatmap,
                ")")),
            need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                    input$end_n_heatmap > input$start_n_heatmap,
                paste0("Please select an end value greater than your start value (",
                    input$start_n_heatmap, ") and less than ",
                    dim(reactivevalue$se)[1])),
        )
        results$correlation_heatmap
        }, height = function() {
        session$clientData$output_correlation_heatmap_width
    })

    output$topn_heatmap <- renderPlot({
        validate(need(input$start_n_heatmap >= 1 &&
                input$start_n_heatmap <= (dim(reactivevalue$se)[1] - 1) &&
                input$start_n_heatmap < input$end_n_heatmap,
            paste0("Please enter a start value which is between 1 and ",
                dim(reactivevalue$se)[1],
                " and less than your desired end value (", input$end_n_heatmap,
                ")")),
            need(input$end_n_heatmap <= dim(reactivevalue$se)[1] &&
                    input$end_n_heatmap > input$start_n_heatmap,
                paste0("Please select an end value greater than your start value (",
                    input$start_n_heatmap, ") and less than ",
                    dim(reactivevalue$se)[1])),
        )
        results$topn_heatmap
    }, height = function() {
        session$clientData$output_topn_heatmap_width
    })
})
