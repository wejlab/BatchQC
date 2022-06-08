### PCA TAB ###

## Plot PCA
observeEvent(input$PCA_plot, {
    req(reactivevalue$se)
    validate(need(input$top_n_PCA <= dim(reactivevalue$se)[1] &&
                      input$top_n_PCA > 1, "Please select a value for the top
                variable features to use that falls within 2 and the size of
                your dataset"))
    assays <- input$pca_assays
    msg <- sprintf('Generating plot for: %s...', paste(assays, collapse=', '))
    withProgress(message=msg, {
        results<-PCA_plotter(reactivevalue$se,
                            input$top_n_PCA,
                            input$variates_color,
                            input$variates_shape,
                            assays)
        setProgress(.8, 'Displaying figure...')
        output$PCA <- renderPlot({validate(need(input$top_n_PCA <=
                                                   dim(reactivevalue$se)[1] &&
                                                   input$top_n_PCA > 1,
                                               "Please select a value for the top
                                           variable features to use that falls
                                           within 2 and the size of your
                                           dataset"))
            results$plot})
        output$var_explained <- renderTable(
            results$var_explained, rownames = TRUE, digits = 4)
        setProgress(1, 'Complete.')
    })
})
