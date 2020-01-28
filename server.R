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
  
  country_df <- reactive({
    country_df = region_df() %>% 
      filter(country == input$chart_country)
  })
  
  ### Filter country selection based on region input
  observeEvent(input$chart_region,{
    updateSelectInput(session,'chart_country',
                      choices = unique(region_df()$country[region_df()$region == input$chart_region]))
  })
  
  ## Trends ####################
  output$trend_country <- renderPlotly({
    plot_ly(country_df(),
            x = ~year, y = ~composite.index) %>% 
      add_lines() %>% 
      layout(autotick = FALSE, dtick = 1,
             xaxis = list(title = "Year", tickvals = 1989:2014), 
             yaxis = list(title = "Indicator on a scale of 0 to 10", tickvals = 0:10))
  })
  
  output$trends <- renderPlotly({
    plot_ly(region_df(),
            x = ~year, y = ~composite.index) %>% 
      add_lines(linetype = ~country) %>% 
      layout(autotick = FALSE, dtick = 1,
             xaxis = list(title = "Year", tickvals = 1989:2014), 
             yaxis = list(title = "Indicator on a scale of 0 to 10", tickvals = 0:10))
  })
  
  ## Distribution ############### 
  output$distr <- renderPlotly({
    ggplotly(region_df() %>% 
               ggplot(aes(x = composite.index)) +
               geom_freqpoly(stat = "density",
                             aes(color = region)) +
               xlab("Indicator on a scale of 0 to 10") +
               ylab("Density") +
               scale_x_continuous(limits = c(0, 10), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
             )
  })
  
  output$compr <- renderPlotly({
    df <- conflicts_long %>% filter(indicator == input$chart_indicator)
    ggplotly(df %>% 
               ggplot(aes(x = composite.index)) +
               geom_freqpoly(stat = "density", aes(color = region)) +
               facet_wrap(~region) + 
               xlab("Indicator on a scale of 0 to 10") +
               ylab("Density") + 
               scale_x_continuous(limits = c(0, 10), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  })
  
  output$cormat <- renderPlotly({
    # Select indicators 
    indicators_only <- conflicts_wide %>% select(c(4:31))
    # Melt correlation matrix
    cor_mat <- melt(round(cor(indicators_only, use = 'pairwise.complete.obs'), 2))
    # Create heatmap
    ggplotly(cor_mat %>%
               ggplot(aes(x = Var1, y = Var2, fill = value)) +
               geom_tile() +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45)) +
               scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                    midpoint = 0, limit = c(-1, 1), space = "Lab", 
                                    name="Pearson\nCorrelation") +
               xlab("Indicators") + ylab("Indicators"))
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
