library(shiny)
library(shinydashboard)
library(leaflet)
library(tigris)
library(acs)
library(dplyr)
require(shinyjs)
library(magrittr)
library(ggplot2)
library(plotly)


statedfs <- list()
for (i in 1:50){
  statedfs[[i]] <- list_counties(state.abb[i])
  statedfs[[i]]$state <- rep(state.abb[i], dim(statedfs[[i]])[1])
}
statedf <- do.call("rbind", statedfs)
all_counties <- paste(statedf$county, statedf$state, sep=", ")

Header <- dashboardHeader(
  
)

Sidebar <- dashboardSidebar(
  useShinyjs(),
  selectizeInput("counties", "Select Counties:", all_counties, multiple=TRUE),
  actionButton("GetData", "Get Data")
)

Body <- dashboardBody(
  box(width=NULL,
          leafletOutput("map", height="500")
  ),
  fluidRow(
    column(width=4,plotlyOutput("plot")),
    column(width=4,plotlyOutput("plot_education")),
    column(width=4,plotlyOutput("plot_housing"))
  )
)
#  br(),
#  column(8,leafletOutput("map", height="600px")),
#  column(4,br(),br(),br(),br(),plotOutput("plot", height="300px")),
#  br()




dashboardPage(Header, Sidebar, Body)