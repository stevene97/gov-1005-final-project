rm(list = ls())
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)


server <- function(input, output) { 
  
  output$ui <- renderUI({
    
    check1 <- input$Tabs == "tabs"
    check2 <- input$MoreTabs == "moretabs"
    
    if(length(check1)==0){check1 <- F}
    if(length(check2)==0){check2 <- F}
    
    if(check1 && check2){
      tabBox(title = "intro",id= "ttabs", width = 8, height = "420px",
             tabPanel("Files", dataTableOutput("Files")),
             tabPanel("Files1", dataTableOutput("Files1"))
      )
    }
    else if(check1){
      tabBox(title = "intro",id= "ttabs", width = 8, height = "420px",tabPanel("Files", dataTableOutput("Files")))
    }
    else{return(NULL)}
  })
}

shinyApp(ui, server)