### DIFFERENTIAL EXPRESSION ANALYSIS TAB ###
observeEvent(input$DE_batch, {
    req(reactivevalue$se, input$DE_batch)
    DE_covariate_choices <- covariates_not_confounded(reactivevalue$se,
                                                   input$DE_batch)
    updateSelectizeInput(session = session, inputId = "DE_conditions",
                         choices = DE_covariate_choices, selected = NULL)
})

## Update pvalue summary table
pvals_summary_reactive <- eventReactive(input$DE_analyze, {
    req(input$DE_batch, input$DE_conditions, input$DE_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        pval_summary_table <- pval_summary(reactivevalue$se, input$DE_batch,
                                           input$DE_conditions,
                                           input$DE_assay)
        pval_summary_table$pval_table
    })
})

## Update batch pvalue boxplot
batch_pvals_reactive <- eventReactive(input$DE_analyze, {
    req(input$DE_batch, input$DE_conditions, input$DE_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        plot_batch_pvals <- batch_pval_plotter(reactivevalue$se,
                                               input$DE_batch,
                                               input$DE_conditions,
                                               input$DE_assay)
        plot_batch_pvals$batch_boxplot
    }, error = function(err) {
    })
})

## Update covariate pvalue boxplot
covariate_pvals_reactive <- eventReactive(input$DE_analyze, {
    req(input$DE_batch, input$DE_conditions, input$DE_assay,
        reactivevalue$se)
    # Create boxplot for batch pvals
    tryCatch({
        plot_covariate_pvals <- covariate_pval_plotter(reactivevalue$se,
                                                       input$DE_batch,
                                                       input$DE_conditions,
                                                       input$DE_assay)
        plot_covariate_pvals$covar_boxplot
    }, error = function(err) {
    })
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

observeEvent(input$DE_analyze, {
    req(reactivevalue$se, input$DE_analyze)
    
    results <- DE_analyze(reactivevalue$se, input$DE_method, 
                          input$DE_conditions, input$DE_assay)
    output$DE_results <- renderDT({results$res
    })
    output$volcano <- renderPlot({
        volcano_plot(results$volcano,input$slider)
    }, height = function() {session$clientData$output_volcano_width
    })
    
    output$downloadDEData <- downloadHandler(
        filename = function() {
            paste("DE_results", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(results$res,file)
        }
    )
    
})


