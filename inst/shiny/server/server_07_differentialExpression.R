### DIFFERENTIAL EXPRESSION ANALYSIS TAB ###
observeEvent(input$DE_batch, {
    req(reactivevalue$se, input$DE_batch)
    DE_covariate_choices <- covariates_not_confounded(reactivevalue$se,
        input$DE_batch)
    updateSelectizeInput(session = session, inputId = "DE_conditions",
        choices = DE_covariate_choices, selected = NULL)
})

observeEvent(input$DE_analyze, {
    req(reactivevalue$se, input$DE_analyze)

    withProgress({
        setProgress(0.5, 'Calculating...')
        reactivevalue$DE_results <- DE_analyze(reactivevalue$se,
            input$DE_method, input$DE_batch, input$DE_conditions,
            input$DE_assay)
        setProgress(1, 'Complete!')
    })
### figure this stuff out
    display_covariate <- names(reactivevalue$DE_results)[length(reactivevalue$DE_results)] #the last item in resultsNames list
    #pval_summary_table <- pval_summary(reactivevalue$DE_results)
    # volcano <- abind(DESeq2::results(reactivevalue$DE_results$dds,
    #     name = display_covariate)$log2FoldChange,
    #     DESeq2::results(reactivevalue$DE_results$dds,
    #         name = display_covariate)$pvalue,
    #     rownames(DESeq2::results(reactivevalue$DE_results$dds,
    #         name = display_covariate)), along = 2)
    # volcano <- as.data.frame(volcano)
    # volcano[, 1:2] <- sapply(volcano[, 1:2], as.numeric)
    # results_tab <- as.data.frame(DESeq2::results(reactivevalue$DE_results$dds,
    #     name = display_covariate))

    output$DE_results <- renderDT({
        reactivevalue$DE_results[[length(reactivevalue$DE_results)]]
    }) #this only displays the results for the last analysis; need to update to include all analysis

    output$pval_summary <- renderDT({pval_summary(reactivevalue$DE_results)}) #,
    #     rownames = TRUE,
    #     striped = TRUE,
    #     bordered = TRUE,
    #     caption = "<b> <span style='color:#000000'> P-Value Summary Table </b>",
    #     caption.placement = getOption("xtable.caption.placement", "top"),
    # )

    output$covariate_pval_plot <- renderPlot({
        covariate_pval_plotter(reactivevalue$DE_results)
    })

    #output$batch_pval_plot <- renderPlot({
    #    batch_pval_plotter(reactivevalue$DE_results$dds)
    #})

    # output$volcano <- renderPlotly({
    #     volcano_plot(reactivevalue$DE_results[[length(names(reactivevalue$DE_results))]], input$pslider, input$fcslider)
    # })

    output$downloadDEData <- downloadHandler(
        filename = function() {
            paste("DE_results", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(reactivevalue$DE_results[[length(reactivevalue$DE_results)]], file)
        }
    )

    updateSelectizeInput(session = session,
        inputId = "DE_res_selected",
        choices = names(reactivevalue$DE_results),
        selected = names(reactivevalue$DE_results)
            [length(reactivevalue$DE_results)])
     updateSliderInput(session = session,
         inputId = "fcslider",
         min = round(min(abs(reactivevalue$DE_results[[length(reactivevalue$DE_results)]][, 1]))), #no longer using same volcano object
         max = round(max(abs(reactivevalue$DE_results[[length(reactivevalue$DE_results)]][, 1]))),
         value = round((max(abs(reactivevalue$DE_results[[length(reactivevalue$DE_results)]][, 1])) + min(abs(reactivevalue$DE_results[[length(reactivevalue$DE_results)]][, 1]))) / 2))
})

plotVolcanoPlotButton <- eventReactive(input$volcano_plot, {
    volcano_plot(reactivevalue$DE_results[[as.character(input$DE_res_selected)]], input$pslider, input$fcslider)
})

output$volcano <- renderPlotly({
    plotVolcanoPlotButton()
})

# observeEvent(input$DE_res_selected, {
#     updateSliderInput(session = session,
#         inputId = "fcslider",
#         min = round(min(abs(reactivevalue$DE_results[[as.character(input$DE_res_selected)]][, 1]))),
#         max = round(max(abs(reactivevalue$DE_results[[as.character(input$DE_res_selected)]][, 1]))),
#         value = round(max(abs(reactivevalue$DE_results[[as.character(input$DE_res_selected)]][, 1])) + min(abs(reactivevalue$DE_results[[length(as.character(input$DE_res_selected))]][, 1])) / 2))
#
# })
# observeEvent(input$DE_res_selected, {
#     req(reactivevalue$DE_results)
#
#     display_covariate <- as.character(input$DE_res_selected)
#
#     # volcano <- abind(DESeq2::results(reactivevalue$DE_results$dds,
#     #     name = display_covariate)$log2FoldChange,
#     #     DESeq2::results(reactivevalue$DE_results$dds,
#     #         name = display_covariate)$pvalue,
#     #     rownames(DESeq2::results(reactivevalue$DE_results$dds,
#     #         name = display_covariate)), along = 2)
#     # volcano <- as.data.frame(volcano)
#     # volcano[, 1:2] <- sapply(volcano[, 1:2], as.numeric)
#     # results_tab <- as.data.frame(DESeq2::results(reactivevalue$DE_results$dds,
#     #     name = display_covariate))
#
#     output$DE_results <- renderDT({
#         reactivevalue$DE_results[[display_covariate]]
#     })
#
#     output$volcano <- renderPlotly({
#         volcano_plot(reactivevalue$DE_results[[display_covariate]], input$pslider, input$fcslider)
#     })
#
#     output$downloadDEData <- downloadHandler(
#         filename = function() {
#             paste("DE_results", ".csv", sep = "")
#         },
#         content = function(file) {
#             write.csv(reactivevalue$DE_results[[display_covariate]], file)
#         }
#     )
# })
