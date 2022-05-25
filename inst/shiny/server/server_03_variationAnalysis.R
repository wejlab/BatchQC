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

## Update pvalue summary table
pvals_summary_reactive <- eventReactive(input$variation, {
    req(input$variation_batch, input$variation_condition, input$variation_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        pval_summary_table <- pval_summary(reactivevalue$se, input$variation_batch,
                                           input$variation_condition,
                                           input$variation_assay)
        pval_summary_table$pval_table
    })
})

## Update batch pvalue boxplot
batch_pvals_reactive <- eventReactive(input$variation, {
    req(input$variation_batch, input$variation_condition, input$variation_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        plot_batch_pvals <- batch_pval_plotter(reactivevalue$se,
                                               input$variation_batch,
                                               input$variation_condition,
                                               input$variation_assay)
        plot_batch_pvals$batch_boxplot
    }, error = function(err) {
    })
})

## Update covariate pvalue boxplot
covariate_pvals_reactive <- eventReactive(input$variation, {
    req(input$variation_batch, input$variation_condition, input$variation_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        plot_covariate_pvals <- covariate_pval_plotter(reactivevalue$se,
                                                       input$variation_batch,
                                                       input$variation_condition,
                                                       input$variation_assay)
        plot_covariate_pvals$covar_boxplot
    }, error = function(err) {
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

        output$pval_summary <- renderTable({
            pvals_summary_reactive()},
            rownames = TRUE,
            striped = TRUE,
            bordered = TRUE,
            caption = "<b> <span style='color:#000000'> P-Value Summary Table </b>",
            caption.placement = getOption("xtable.caption.placement","top"),
        )

        output$batch_pval_plot <- renderPlot({
            batch_pvals_reactive()
        })

        output$covariate_pval_plot <- renderPlot({
            covariate_pvals_reactive()
        })
    })
})
