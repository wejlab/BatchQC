### DIFFERENTIAL EXPRESSION ANALYSIS TAB ###
observeEvent(input$DE_analyze, {
    req(reactivevalue$se, input$DE_analyze)
    
    results <- analyze_SE(reactivevalue$se, input$DE_method, 
                          input$DE_conditions, input$DE_assay)
    output$DE_results <- renderDT({results$res
    })
    output$volcano <- renderPlot({
        volcano_plot(results$volcano,input$slider)
    }, height = function() {session$clientData$output_volcano_width
    })
    
})


