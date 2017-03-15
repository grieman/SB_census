library(shiny)
library(shinydashboard)
library(leaflet)
library(tigris)
library(acs)
library(dplyr)
require(shinyjs)
library(magrittr)

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
  leafletOutput("map", height="600px")
)



dashboardPage(Header, Sidebar, Body)