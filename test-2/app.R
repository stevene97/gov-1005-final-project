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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Sanitary Violations"),
  dashboardSidebar(
    selectizeInput("name",
                   label = "Type the name of a restaurant",
                   choices = sort(unique(df$first_estab)),
                   selected = 'Annenberg Dining Hall'),
    selectInput("violation",
                label = 'Select violation',
                choices = c("All", sort(unique(df$code_description))))
    
  ),
  
  dashboardBody(
    fluidRow(
      box(title = paste('Sanitary Violations Over Time For Establishment'),
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
      box(title = 'Pie Chart with Different Violations',
          width = 5,
          solidHeader = TRUE,
          status = 'primary',
          plotlyOutput('pie')),
      
      box(title = 'Table',
          width = 7,
          height=6,
          solidHeader = TRUE,
          status = 'primary',
          div(style = 'height:400px; overflow-y: scroll', DT::dataTableOutput('table'))
      ))
  )
)


server <- function(input, output) {
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
  
  # Same as above, but with fill=TRUE
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
  
  output$pie <- renderPlotly({
    df %>% 
      filter(first_estab == input$name) %>% 
      count(code_description) %>% 
      arrange(desc(n)) %>% 
      head(10) %>% 
      plot_ly(labels = ~code_description, values = ~n, type = 'pie', showlegend = FALSE)
  })
  
  filteredData <- reactive({
    df_out <- df
    
    if(input$violation == 'All'){
      df_out <- df %>% 
        filter(first_estab == input$name) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    
    if(input$violation != "All"){
      df_out <- df %>% 
        filter(first_estab == input$name) %>% 
        filter(code_description == input$violation) %>% 
        mutate(cited_date = as.Date(cited_date)) %>% 
        mutate(corrected_date = as.Date(corrected_date)) %>% 
        select(first_estab, cited_date, corrected_date, code_description)
      
      colnames(df_out) <- c("Name", "Cited Date", "Corrected Date", "Description")
    }
    df_out
  })
  
  output$table <- DT::renderDataTable({
    filteredData()
  })
  
}

shinyApp(ui, server)