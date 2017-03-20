library(shiny)
library(leaflet)
library(tigris)
library(acs)
library(dplyr)
require(shinyjs)
library(magrittr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  spatialdata <- eventReactive(input$GetData, {
    counties <- input$counties
    states <- counties %>% strsplit(", ") %>% lapply('[[',2) %>% unlist()
    counties %<>% strsplit(", ") %>% lapply('[[',1) %>% unlist()
    countiesdf <- data.frame(counties, states)
    
    split_counties <- split(countiesdf, countiesdf$states)
    
    split_counties %<>% lapply(function(x){
      tracts(state=as.character(x$states[1]),
             county=as.character(x$counties), cb=TRUE)
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
        geo.make(state=as.character(x$states[1]),
                 county=as.character(x$counties), tract="*")
      })
      
      geodata <- split_counties[[1]]
      
      if(length(split_counties) > 1){
        for (i in 2:length(split_counties)){
          if (i==2){geodata <- split_counties[[1]] + split_counties[[2]]}
          else {geodata <- geodata + split_counties[[i]]}
        }
      }
      
      return(geodata)
  })
  
  income_df <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    #change data to change data's year
    income <- acs.fetch(endyear = 2015, span = 5, geography = geodata, table.number = "B19001", col.names = "pretty")
  
    above150K <- rowSums(income@estimate[,16:17])
    
    income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                                   str_pad(income@geography$county, 3, "left", pad="0"), 
                                   str_pad(income@geography$tract, 6, "left", pad="0")), 
                            income@estimate,
                            above150K, 
                            stringsAsFactors = FALSE)
  })
  
  income_df_lag1 <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    #change data to change data's year
    income <- acs.fetch(endyear = 2015-1, span = 5, geography = geodata, table.number = "B19001", col.names = "pretty")
    income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                                   str_pad(income@geography$county, 3, "left", pad="0"), 
                                   str_pad(income@geography$tract, 6, "left", pad="0")), 
                                  income@estimate,stringsAsFactors = FALSE)
  })
    
  income_merged <- eventReactive(input$GetData, {
    spatialdata <- spatialdata(); income_df2 <- income_df()
    #income_df2 <- select(income_df, c(1,2,19))
    rownames(income_df2)<-1:nrow(income_df2)
    names(income_df2)[c(1,2,19)] <-c("GEOID", "total", "over_150")
    income_df2$percent <- 100*(income_df2$over_150/income_df2$total)
    income_merged<- geo_join(spatialdata, income_df2, "GEOID", "GEOID")
    income_merged[income_merged$ALAND>0,]  
  })
  
  popup <- eventReactive(input$GetData, {
    paste0("GEOID: ", income_merged()$GEOID, "<br>", "Percent of Households above $150k: ", round(income_merged()$percent,2))
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
                  fillOpacity = 0.6, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup(), layerId = income_merged()$GEOID) #%>%
      #addLegend(pal = pal(), 
      #          values = income_merged()$percent, 
      #          position = "bottomright", 
      #          title = "Percent of Households<br>above $150k",
      #          labFormat = labelFormat(suffix = "%"))
  })
  
  
  #### Click Map ####
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click$id
  })
  
  
  output$plot=renderPlot({
    p <- input$map_shape_click$id
    if(is.null(p)){p=income_merged()$GEOID[1]}
    data <- income_merged()[c(5,11:26)] %>% subset(GEOID == p) %>% as.data.frame()
    data[c(3:17)] <- data[c(3:17)] / data$total
    data <- data[-c(1,2)] %>% t() %>% as.data.frame()
    colnames(data) <- c("Percentage")
    data$bin <- c("Less than 10,000", "10,000 to 14,999","15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", "35,000 to 39,999", "40,000 to 44,999", "45,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 to 99,999", "100,000 to 124,999", "125,000 to 149,999", "150,000 to 199,999") %>% as.factor()
    data$order <- 1:length(data$bin)
    data %>% 
      ggplot(aes(x=reorder(bin, order), y=Percentage)) + geom_col() + 
      theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 70, hjust = 1))
  })
  
  
})
