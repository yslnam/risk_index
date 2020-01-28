# ui.R

# Layout
dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "Macro Risks Index",
        tags$li(actionLink("LinkedIn", 
                           label = "", 
                           icon = icon("linkedin"),
                           onclick = "window.open('https://www.linkedin.com/in/yousun-nam/')"),
                class = "dropdown"),
        tags$li(actionLink("GitHub", 
                           label = "", 
                           icon = icon("github"),
                           onclick = "window.open('https://github.com/yslnam/risk_index')"),
                class = "dropdown")
    ),
    dashboardSidebar(
        sidebarUserPanel("You-Sun Nam",
                          subtitle = "Data Science Fellow",
                          image = "profile.jpg"),
        
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Charts", tabName = "chart", icon = icon("bar-chart-o")),
            menuItem("Data", tabName = "data", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "map",
                fluidRow(
                    h1("Visualizing Macro Risk Indicators")
                ),
                fluidRow( # start fluidRow for imap
                    leafletOutput("imap")
                ), #end fluidRow for imap
                fluidRow(
                    column(4, 
                           selectInput(inputId = "select_indicator",
                                       label = h4("Indicator"),
                                       choices = ind_vars,
                                       selected = 'food')),
                    column(4,
                           selectInput(inputId = "select_year",
                                       label = h4("Year"),
                                       choices = c(1989, 2014),
                                       selected = '2004')),
                    column(4,
                           selectInput(inputId = "select_hue",
                                       label = h4("Color palette"),
                                       choices = c("Blues", "BuGn", "BuPu",
                                                     "GnBu", "Greens", "Oranges",
                                                     "OrRd", "PuBu", "PuBuGn",
                                                     "PuRed", "PuBuGn", "PuRd",
                                                     "Purples", "RdPu", "Reds",
                                                     "YlGn", "YlGnBu", "YlGnBr",
                                                     "YlOrRd"),
                                       selected = "YlGnBu"))
                ) # end fluidRow for user input
            ), # end tabItem for map
            tabItem(tabName = "chart",
                    fluidRow(
                        column(2, h3("A closer look:")),
                        column(2,
                               selectInput(inputId = "chart_region", 
                                           label = h4("Region"),
                                           choices = list("Americas", "Africa",
                                                          "Asia", "Europe", "Oceania"),
                                           selected = "Africa")
                               ),
                        column(2,
                               selectInput(inputId = "chart_country",
                                           label = h4("Country"),
                                           choices = NULL)
                               ),
                        column(2,
                               selectInput(inputId = "chart_indicator",
                                           label = h4("Indicator"),
                                           choices = ind_vars,
                                           selected = 'ineq_swiid')
                               ),
                        column(3,
                               sliderInput(inputId = "chart_year",
                                           label = h4("Year Range"),
                                           min = 1989,
                                           max = 2014,
                                           value = c(1989, 2014),
                                           sep = "")
                               ) # end last column, trends indicator selection
                    ), # end fluidRow 1 of chart
                    fluidRow(
                        tabBox(title = "Annual Trends by Country or Region",
                               width = 12,
                               tabPanel(title = "Country", plotlyOutput("trend_country")),
                               tabPanel(title = "Region", plotlyOutput("trends")))
                    ),
                    fluidRow(
                        tabBox(title = "Distribution by Region",
                               width = 12,
                               tabPanel(title = "Region", plotlyOutput("distr")),
                               tabPanel(title = "Comparison to other regions", plotlyOutput("compr")))
                    ),
                    fluidRow(
                        tabBox(title = "Relationships between Indicators",
                               width = 12,
                               tabPanel(title = "Correlation Matrix", plotlyOutput("cormat")))
                    )
                    ), # end tabItem for chart
            tabItem(tabName = "data",
                    fluidRow(
                        tabBox(title = "Data sets",
                               width = 12,
                               tabPanel(title = "Original dataset", DT::dataTableOutput("dataset1")),
                               tabPanel(title = "Merged dataset", DT::dataTableOutput("dataset2")))
                    ), # end fluidRow for tabItem 'data'
                    fluidRow(
                        box(title = "Note",
                            status = "warning",
                            "The merged dataset seen here is only a sample. The full merged dataset is available for download on my GitHub.",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12),
                        box(title = "Attribution",
                            status = "primary",
                            "1. European Commission Joint Research Centre. 'Global Conflict Risk Index, Version July 2017' (2017),",
                            br(),
                            "Retrieved from http://conflictrisk.jrc.ec.europa.eu/. Accessed through Resource Watch. https://www.resourcewatch.org/.",
                            p(),
                            "2. lukes, 'ISO-3166 Countries with Regional Codes' (2019[2017]),",
                            br(),
                            "GitHub repository, https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/.",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12)
                        )
                    ) # end tabItem data
        ) # end tabItems
    ) # end dashboardBody
)
