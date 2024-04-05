### EXPERIMENTAL DESIGN TAB ###

## Update experimental design
observeEvent(input$design_covariate, {
    req(input$design_batch, input$design_covariate, reactivevalue$se)
    # Create batch design table
    design <- batch_design(reactivevalue$se, input$design_batch,
                           input$design_covariate)
    output$batch_design <- renderTable(design)
})

## Update confounding design table
observeEvent(input$design_batch, {
    req(input$design_batch, reactivevalue$se)
    conf_stats <- confound_metrics(reactivevalue$se, input$design_batch)
    output$confounding_table <- renderTable(conf_stats, rownames = TRUE)
})
