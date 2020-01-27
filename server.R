# server.R

function(input, output, session) {

  # Interactive map #################################
  observe({
    output$imap <- renderLeaflet({
      gcri_leaflet(gc.indicator = input$select_indicator,
                   gc.year = input$select_year,
                   hue = input$select_hue)
    })
  })
  
  # Charts ##########################################
  ## Filtered data ##############
  region_df <- reactive({
    region_df = conflicts_long %>% 
      filter(region == input$chart_region & indicator == input$chart_indicator) %>% 
      filter(between(year, input$chart_year[1], input$chart_year[2]))
  })
  
  ## Trends ####################
  output$trends <- renderPlotly({
    plot_ly(region_df(),
            x = ~year, y = ~composite.index) %>% 
      add_lines(linetype = ~country)
  })
  
  ## Distribution ############### 
  output$distr <- renderPlotly({
    ggplotly(region_df() %>% 
               ggplot(aes(x = composite.index)) +
               geom_freqpoly(stat = "density",
                             aes(color = region))
             )
  })
  
  output$compr <- renderPlotly({
    df <- conflicts_long %>% filter(indicator == input$chart_indicator)
    ggplotly(df %>% 
               ggplot(aes(x = composite.index)) +
               geom_freqpoly(stat = "density", aes(color = region)) +
               facet_wrap(~region))
  })
  
  output$cormat <- renderPlotly({
    indicators_only <- conflicts_wide %>% select(c(4:31))
    cor_mat <- melt(round(cor(indicators_only, use = 'pairwise.complete.obs'), 2))
    ggplotly(cor_mat %>% 
               ggplot(aes(x = Var1, y = Var2, fill = value)) +
               geom_tile() +
               scale_fill_gradient2(low = "blue", high = "red", mid = "white"))
  })
             
  # Data set(s) ######################################
  conflicts_long2 = conflicts_long[sample(nrow(conflicts_long), 1000), ]
  
  output$dataset1 <- DT::renderDataTable({
    DT::datatable(conflicts_wide, options = list(scrollX = TRUE))
  })
  
  output$dataset2 <- DT::renderDataTable({
    DT::datatable(conflicts_long2, options = list(scrollX = TRUE))
  })
  
}
