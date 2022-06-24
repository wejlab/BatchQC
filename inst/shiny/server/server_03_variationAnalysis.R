### VARIATION ANALYSIS TAB ###

## Update covariate options to only those that are not confounded with batch
observeEvent(input$variation_batch, {
    req(reactivevalue$se, input$variation_batch)
    covariate_choices <- covariates_not_confounded(reactivevalue$se,
                                                   input$variation_batch)
    updateSelectizeInput(session = session, inputId = "variation_condition",
                         choices = covariate_choices, selected = NULL)
})

## Update variation analysis plot
ev_plot_reactive <- eventReactive(input$variation, {
    req(input$variation_batch, input$variation_condition, input$variation_assay,
        reactivevalue$se)
    # Create boxplot for variation explained by batch, condition, and
    # batch + condition
    tryCatch({
        batchqc_ev_plot <- EV_plotter(reactivevalue$se, input$variation_batch,
                                      input$variation_condition,
                                      input$variation_assay)
        plot(batchqc_ev_plot$EV_boxplot)
    })
})

## Update variation analysis table
ev_table_reactive <- eventReactive(input$variation, {
    req(input$variation_batch, input$variation_condition,
        input$variation_assay, reactivevalue$se)
    # Create boxplot for variation explained by batch, condition,
    # and batch + condition
    tryCatch({
        batchqc_ev_table <- EV_table(reactivevalue$se, input$variation_batch,
                                     input$variation_condition,
                                     input$variation_assay)
        batchqc_ev_table$EV_table
    }, error = function(err) {
        showNotification("At least one covariate is confounded with another!
                     Please choose different covariates.", type = "error")
        print("At least one covariate is confounded with another!
          Please choose different covariates.")
    })
})

## Display variation and p-value plots and tables
observeEvent(input$variation, {
    withBusyIndicatorServer("variation", {
        output$EV_show_plot <- renderPlot({
            ev_plot_reactive()
        })

        output$EV_show_table <- renderDataTable({
            ev_table_reactive()
        })
    })
})
