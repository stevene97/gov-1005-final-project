library(shiny)
library(leaflet)
library(plotly)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(kableExtra)
library(DT)

# Loading in the data
df <- read_rds("data.rds")

# Using the "navbar" page to allow for different tabs:
  # ANALYZE: Analyze focuses on sanitary violations for one specific establishment in Harvard Square.
  # COMPARE: Compare allows the user to compare two different food establishments' sanitary violations over time.
  # SUMMARIZE: Summarize allows the user to compare multiple establishments' sanitary violations, seeing which are the       worst violators either overall or for a specific violation.

# Step 1: Building the interface using the "navbar" Shiny layout.
ui <- navbarPage("Sanitary Violations",
                 
                 # Tab One: Analyzing a specific food establishment in Harvard Square
                 tabPanel("Analyze",
                          titlePanel(""),
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("name",
                                             label = "Type the name of a restaurant",
                                             choices = sort(unique(df$first_estab)),
                                             selected = 'Annenberg Dining Hall'),
                              width=3),
                            mainPanel(
                              fluidRow(
                                box(title = 'Sanitary Violations Over Time For Establishment',
                                    width = 7,
                                    solidHeader = TRUE,
                                    status = 'primary',
                                    plotlyOutput('linegraph')),
                                
                                box(title = 'Location',
                                    width = 5,
                                    solidHeader = TRUE,
                                    status = 'primary',
                                    leafletOutput('location'))),
                              fluidRow(
                                box(title = 'Table',
                                    width = 12,
                                    solidHeader = TRUE,
                                    status = 'primary',
                                    DT::dataTableOutput('table')
                                )
                              )
                            )
                          )),
                 
                 # Tab Two: Comparing two establishments in Harvard Square
                 tabPanel("Compare",
                          titlePanel(""),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("compare1",
                                          label = "Type the name of a restaurant",
                                          choices = sort(unique(df$first_estab)),
                                          selected = 'Felipe\'s Taqueria'),
                              selectInput("compare2",
                                          label = "Type the name of a restaurant",
                                          choices = sort(unique(df$first_estab)),
                                          selected = 'El Jefe\'s Taqueria'),
                              selectInput("compare3",
                                          label = "Filter bar graph and tables for a specific violation",
                                          choices = c("All", sort(unique(df$code_description))),
                                          selected = 'All'),
                              width=3),
                            mainPanel(
                              fluidRow(
                                box(title = 'Sanitary Violations Over Time',
                                    width = 6,
                                    solidHeader = TRUE,
                                    status = 'primary',
                                    plotlyOutput('linegraph2')),
                                box(title = 'Compare',
                                    width = 6,
                                    solidHeader = TRUE,
                                    status = 'primary',
                                    plotlyOutput('bargraph_compare')),
                                fluidRow(
                                  box(title = 'Citations',
                                      width = 6,
                                      solidHeader = TRUE,
                                      status = 'primary',
                                      DT::dataTableOutput('table_compare1')
                                  ),
                                  box(title = 'Citations',
                                      width = 6,
                                      solidHeader = TRUE,
                                      status = 'primary',
                                      DT::dataTableOutput('table_compare2'))
                                )
                              )
                            ))),
                 
                 # Tab three: Overall summary of sanitary violations in food establishments around Harvard
                 tabPanel("Summarize",
                          titlePanel(""),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("summarize",
                                          label = "Select a violation",
                                          choices = c("All", sort(unique(df$code_description))),
                                          selected = 'All'),
                              width=3),
                            mainPanel(
                              leafletOutput("summary_map")
                            )))
)

#------ DEFINING THE SERVER LOGIC: Outputs to be displayed on the shiny page ------
server <- function(input, output) {
  #------ "ANALYZE" PAGE OUTPUT --------
  # Output 1: This output will show a line graph comparing trends over time for a specific food establishment.
  output$linegraph <- renderPlotly({
    df %>% 
      filter(first_estab == input$name) %>% 
      mutate(year_cited = as.Date(cited_date)) %>% 
      count(year_cited) %>% 
      plot_ly(x = ~year_cited, y = ~n, type = 'area') %>% 
      layout(title = paste(''),
             xaxis = list(title = 'Date Cited'),
             yaxis = list(title = 'Number of Violations'))
  })
  
  # Output 2: This is the part that will output the Leaflet map showing the location of the establishment.
  output$location <- renderLeaflet({
    df %>% 
      filter(first_estab == input$name) %>%
      as.tibble() %>%
      mutate(lng = as.numeric(as.character(lng))) %>% 
      mutate(lat = as.numeric(as.character(lat))) %>% 
      head(1) %>% 
      leaflet() %>% 
      addProviderTiles('CartoDB',
                       options = providerTileOptions(minZoom = 16, maxZoom = 18)) %>% 
      addPopups(~lng, ~lat, input$name)
  })
  
  # Output 3: This part will show a table that lists each of the sanitary violations that a food establishment has. This is defined by the reactive element immediately below, as well as the DT::renderDataTable({}) output below the reactive part.
  filteredData <- reactive({
    df_out <- df %>% 
      filter(first_estab == input$name) %>% 
      mutate(cited_date = as.Date(cited_date)) %>% 
      mutate(corrected_date = as.Date(corrected_date)) %>% 
      select(first_estab, cited_date, corrected_date, code_description)
    
    colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    
    df_out
  })
  
  output$table <- DT::renderDataTable({
    filteredData()
  })
  
  #----------- "COMPARE" PAGE OUTPUT ---------------
  # Output 1: This line graph will compare trends over time for two establishments around Harvard Square
  output$linegraph2 <- renderPlotly({
    df %>% 
      filter(first_estab == input$compare1 | first_estab == input$compare2) %>% 
      mutate(year_cited = as.Date(cited_date)) %>% 
      count(first_estab, year_cited) %>% 
      plot_ly(x = ~year_cited, y = ~n, type = 'area', color = ~first_estab) %>% 
      layout(xaxis = list(title = 'Date'), 
             yaxis = list(title = 'Number of Violations'),
             showlegend = F)
  })
  
  # Output 2: This bar graph will compare trends over time for two establishments around Harvard Square for a specific violation.
  output$bargraph_compare <- renderPlotly({
    if(input$compare3 == 'All'){
      df %>% 
        filter(first_estab == input$compare1 | first_estab == input$compare2) %>% 
        mutate(year_cited = as.Date(cited_date)) %>% 
        count(first_estab, year_cited) %>% 
        group_by(first_estab) %>% 
        summarize(sum = sum(n)) %>% 
        plot_ly(x = ~first_estab, y = ~sum, type = 'bar', color = ~first_estab) %>% 
        layout(xaxis = list(title = 'Establishment'), 
               yaxis = list(title = 'Number of Violations'),
               showlegend = FALSE)
    }
    
    else if(input$compare3 != 'All'){
      df %>% 
        filter(first_estab == input$compare1 | first_estab == input$compare2) %>% 
        filter(code_description == input$compare3) %>% 
        mutate(year_cited = as.Date(cited_date)) %>% 
        count(first_estab, year_cited) %>% 
        group_by(first_estab) %>% 
        summarize(sum = sum(n)) %>% 
        plot_ly(x = ~first_estab, y = ~sum, type = 'bar', color = ~first_estab) %>% 
        layout(xaxis = list(title = 'Establishment'), 
               yaxis = list(title = 'Number of Violations'),
               showlegend = FALSE)
    }
  })
  
  # Outputs 3 and 4: Tables showing each establishments' respective violations. Each of these will have an output for the kable immediately preceded by a reactive function that filters the data accordingly using a similar process as above.
  filteredData_compare1 <- reactive({
    if(input$compare3 == 'All'){
      df_out <- df %>% 
        filter(first_estab == input$compare1) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    
    else if(input$compare3 != 'All'){
      df_out <- df %>% 
        filter(first_estab == input$compare1) %>% 
        filter(code_description == input$compare3) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    
    df_out
  })
  
  output$table_compare1 <- DT::renderDataTable({
    filteredData_compare1()
  })
  
  filteredData_compare2 <- reactive({
    if(input$compare3 == 'All'){
      df_out <- df %>% 
        filter(first_estab == input$compare2) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    
    else if(input$compare3 != 'All'){
      df_out <- df %>% 
        filter(first_estab == input$compare2) %>% 
        filter(code_description == input$compare3) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    
    df_out
  })
  
  output$table_compare2 <- DT::renderDataTable({
    filteredData_compare2()
  })
  
  #--------"SUMMARIZE" PAGE OUTPUT------------
  output$summary_map <- renderLeaflet({
    if(input$summarize == 'All'){
      # First: thinking if "All" is selected
      # Step 1: Make a list of all unique first_estabs with corresponding lat, lng values
      coords <- df %>% 
        group_by(first_estab) %>% 
        summarize(lat = lat[1], lng = lng[1])
      
      # Step 2: Make a list of the worst violators
      worst <- df %>% 
        mutate(year_cited = as.Date(cited_date)) %>%
        count(first_estab, year_cited) %>% 
        group_by(first_estab) %>% 
        summarize(sum = sum(n)) %>% 
        arrange(desc(sum)) %>% 
        head(20)
      
      # Step 3: left_join to get lat, lng coordinates
      join_summarize <- left_join(coords, worst, 'first_estab') %>% 
        filter(!is.na(sum)) %>% 
        arrange(desc(sum)) %>% 
        mutate(lat = as.numeric(as.character(lat)), lng = as.numeric(as.character(lng)))
      
      # Step 4: Use the last dataframe to make a map showing the worst violators in Cambridge
      join_summarize %>% 
        leaflet() %>% 
        addProviderTiles('CartoDB') %>% 
        addCircleMarkers(lng = ~lng, lat = ~lat,
                         radius = ~sum/7,
                         color = "red",
                         popup = ~paste("<b>", 
                                        first_estab, 
                                        "</b>: ", 
                                        sum, 
                                        " violations",
                                        sep = ''),
                         fillOpacity = 0.75)
    }
    
    else if(input$summarize != 'All'){
      # Second: thinking if "All" is NOT selected
      # Step 1: Make a list of all unique first_estabs with corresponding lat, lng values
      coords <- joined %>% 
        group_by(first_estab) %>% 
        summarize(lat = lat[1], lng = lng[1])
      
      # Step 2: Make a list of the worst violators, now filtering for a specific violation
      worst <- joined %>% 
        mutate(year_cited = as.Date(cited_date)) %>%
        filter(code_description == input$summarize) %>% 
        count(first_estab, year_cited) %>% 
        group_by(first_estab) %>% 
        summarize(sum = sum(n)) %>% 
        arrange(desc(sum)) %>% 
        head(20)
      
      # Step 3: left_join to get lat, lng coordinates
      join_summarize <- left_join(coords, worst, 'first_estab') %>% 
        filter(!is.na(sum)) %>% 
        arrange(desc(sum)) %>% 
        mutate(lat = as.numeric(as.character(lat)), lng = as.numeric(as.character(lng)))
      
      # Step 4: Use the last dataframe to make a map showing the worst violators in Cambridge
      join_summarize %>% 
        leaflet() %>% 
        addProviderTiles('CartoDB') %>% 
        addCircleMarkers(lng = ~lng, lat = ~lat,
                         radius = ~sum*5,
                         color = "red",
                         popup = ~paste("<b>", 
                                        first_estab, 
                                        "</b>: ", 
                                        sum, 
                                        " violations",
                                        sep = ''),
                         fillOpacity = 10)
      
    }
    
  })
}

shinyApp(ui, server)