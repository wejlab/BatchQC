### DIFFERENTIAL EXPRESSION ANALYSIS TAB ###
observeEvent(input$DE_batch, {
    req(reactivevalue$se, input$DE_batch)
    DE_covariate_choices <- covariates_not_confounded(reactivevalue$se,
        input$DE_batch)
    updateSelectizeInput(session = session, inputId = "DE_conditions",
        choices = DE_covariate_choices, selected = NULL)
})

observeEvent(input$DE_method, {
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
            input$DE_assay) #, input$include_batch, input$condition_of_interest)
        setProgress(1, 'Complete!')
    })

    display_covariate <- names(reactivevalue$DE_results)[length(reactivevalue$DE_results)]

    output$DE_results <- renderDT({
        reactivevalue$DE_results[[length(reactivevalue$DE_results)]]
    }) #this only displays the results for the last analysis; need to update to include all analysis

    output$pval_summary <- renderDT({
        pval_summary(reactivevalue$DE_results)})

    output$covariate_pval_plot <- renderPlot({
        covariate_pval_plotter(reactivevalue$DE_results)
    })

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
        selected = names(reactivevalue$DE_results)[length(
            reactivevalue$DE_results)])
    updateSelectizeInput(session = session,
        inputId = "result_to_view",
        choices = names(reactivevalue$DE_results),
        selected = names(reactivevalue$DE_results)[length(
            reactivevalue$DE_results)])
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

observeEvent(input$DE_res_selected, {
    if (input$DE_res_selected != "") {
        updateSliderInput(session = session,
            inputId = "fcslider",
            min = 0,
            max = round(max(abs(reactivevalue$DE_results[[input$DE_res_selected]][, 1]))),
            value = round(median(abs(reactivevalue$DE_results[[input$DE_res_selected]][, 1]))))

    }
    })

observeEvent(input$result_to_view, {
    output$DE_results <- renderDT({
        reactivevalue$DE_results[[input$result_to_view]]
    })

    output$downloadDEData <- downloadHandler(
        filename = function() {
            paste(input$DE_method, "_results_", input$result_to_view, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(reactivevalue$DE_results[[input$result_to_view]], file)
        }
    )
})
