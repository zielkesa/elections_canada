#app displays winners of polling regions of ridings in the Canadian 2011 federal election
#writen by Stephen Zielke, 2016

library(shiny)

prov.list <- prov <- c("Alberta", "British Columbia", "Manitoba", "Newfoundland and Labrador", 
                      "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", 
                       "Ontario","Prince Edward Island", "Quebec",
                       "Saskatchewan", "Yukon")

shinyUI(pageWithSidebar (
  headerPanel("Federal Election Results"),
  sidebarPanel(
    #drop box of provinces
    selectInput("province","Province",choices=prov.list),
    #drop box of ridings in selected province
    uiOutput("riding.list")
  ),
  
  mainPanel(
    plotOutput("plot")
  )
  
))