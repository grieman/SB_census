library(shiny)
library(leaflet)
library(tigris)
library(acs)
library(dplyr)
require(shinyjs)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  spatialdata <- eventReactive(input$GetData, {
    counties <- input$counties
    states <- counties %>% strsplit(", ") %>% lapply('[[',2) %>% unlist()
    counties %<>% strsplit(", ") %>% lapply('[[',1) %>% unlist()
    countiesdf <- data.frame(counties, states)
    
    split_counties <- split(countiesdf, countiesdf$states)
    
    split_counties %<>% lapply(function(x){
      tracts(state=x$states[1], county=x$counties, cb=TRUE)
    })
    
    rbind_tigris(split_counties)
  })
  
  geodata <- eventReactive(input$GetData, {
      counties <- input$counties
      states <- counties %>% strsplit(", ") %>% lapply('[[',2) %>% unlist()
      counties %<>% strsplit(", ") %>% lapply('[[',1) %>% unlist()
      countiesdf <- data.frame(counties, states)
      
      split_counties <- split(countiesdf, countiesdf$states)
      
      split_counties %<>% lapply(function(x){
        geo.make(state=x$states[1], county=x$counties, tract="*")
      })
      
      geodata <- split_counties[[1]]
      
      #if(length(split_counties > 1)){
      #  for (i in 2:length(split_counties)){
      #    geodata <- geodata + splitcounties[[i]]
      #  }
      #}
  })
  
  income_merged <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    #this always returns autauga AL
    income <- acs.fetch(endyear = 2012, span = 5, geography = geodata, table.number = "B19001", col.names = "pretty")
  
    income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                                   str_pad(income@geography$county, 3, "left", pad="0"), 
                                   str_pad(income@geography$tract, 6, "left", pad="0")), 
                            income@estimate[,c("Household Income: Total:",
                                               "Household Income: $200,000 or more")], 
                            stringsAsFactors = FALSE)
    
    income_df <- select(income_df, 1:3)
    rownames(income_df)<-1:nrow(income_df)
    names(income_df)<-c("GEOID", "total", "over_200")
    income_df$percent <- 100*(income_df$over_200/income_df$total)
    income_merged<- geo_join(spatialdata, income_df, "GEOID", "GEOID")
    income_merged[income_merged$ALAND>0,]  
  })
  
  popup <- eventReactive(input$GetData, {
    paste0("GEOID: ", income_merged()$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged()$percent,2))
  })
  
  pal <- eventReactive(input$GetData, {
    colorNumeric(
      palette = "YlGnBu",
      domain = income_merged()$percent
    )
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = income_merged(), 
                  fillColor = ~pal()(percent), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup()) %>%
      addLegend(pal = pal(), 
                values = income_merged()$percent, 
                position = "bottomright", 
                title = "Percent of Households<br>above $200k",
                labFormat = labelFormat(suffix = "%"))
  })
  
})
