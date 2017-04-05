library(shiny)
library(leaflet)
library(tigris)
library(acs)
library(dplyr)
require(shinyjs)
library(magrittr)
library(ggplot2)
library(reshape2)
library(plotly)

theme_GR <- function(){
  ggplot2::theme(axis.line = ggplot2::element_line(linetype = "solid"),
                 panel.grid.major = ggplot2::element_line(colour = "gray80"),
                 panel.grid.minor = ggplot2::element_line(colour = "gray90",
                                                          linetype = "dashed"), panel.background = ggplot2::element_rect(fill = NA),
                 legend.direction = 'horizontal', legend.position = 'bottom')
}

shinyServer(function(input, output, session) {
  
  progress <- shiny::Progress$new()
  
  observeEvent(input$GetData,{
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    progress$set(message = "Getting data", value = 0)
  })
  
  
  spatialdata <- eventReactive(input$GetData, {
    counties <- input$counties
    states <- counties %>% strsplit(", ") %>% lapply('[[',2) %>% unlist()
    counties %<>% strsplit(", ") %>% lapply('[[',1) %>% unlist()
    countiesdf <- data.frame(counties, states)
    countiesdf$counties %<>% paste("County", sep=" ")
    
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
      countiesdf$counties %<>% paste("County", sep=" ")
      
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
      progress$inc(.1, detail = paste("Geography defined"))
      return(geodata)
  })
  
  income_df <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    income <- acs.fetch(endyear = 2015, span = 5, geography = geodata, table.number = "B19001", col.names = "pretty")
    progress$inc(.3, detail = paste("Income data gathered"))
  
    above150K <- rowSums(income@estimate[,16:17])
    
    income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                                   str_pad(income@geography$county, 3, "left", pad="0"), 
                                   str_pad(income@geography$tract, 6, "left", pad="0")), 
                            income@estimate,
                            above150K, 
                            stringsAsFactors = FALSE)
  })
  
  edu_df <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    edu <- acs.fetch(endyear = 2015, span = 5, geography = geodata, table.number = "B15001", col.names = "pretty")
    progress$inc(.3, detail = paste("Education data gathered"))
    
    gender <- list()
    gender[[1]] <- as.data.frame(edu@estimate)[c(grep("Female", colnames(edu@estimate)))]
    gender[[2]] <- as.data.frame(edu@estimate)[c(grep("Male", colnames(edu@estimate)))]
    for (i in 1:2){
      educlass <- list()
      educlass[[1]] <- gender[[i]][c(grep("Less", colnames(gender[[i]])))]
      educlass[[2]] <- gender[[i]][c(grep("no diploma", colnames(gender[[i]])))]
      educlass[[3]] <- gender[[i]][c(grep("High school", colnames(gender[[i]])))]
      educlass[[4]] <- gender[[i]][c(grep("Some college", colnames(gender[[i]])))]
      educlass[[5]] <- gender[[i]][c(grep("Associate", colnames(gender[[i]])))]
      educlass[[6]] <- gender[[i]][c(grep("Bachelor", colnames(gender[[i]])))]
      educlass[[7]] <- gender[[i]][c(grep("professional", colnames(gender[[i]])))]
      for (j in 1:7){
        educlass[[j]] %<>% rowSums()
      }
      gender[[i]] <- do.call("rbind", educlass) %>% t
      gender[[i]] %<>% prop.table(1) %>% as.data.frame
      colnames(gender[[i]]) <- c("No High School", "Some High School", "High School", "Some College", "Associate's", "Bachelor's", "Postgraduate")
      gender[[i]]$gender <- ifelse(i==1, "Female", "Male")
    }
    
    edu_df <- data.frame(paste0(str_pad(edu@geography$state, 2, "left", pad="0"), 
                                str_pad(edu@geography$county, 3, "left", pad="0"), 
                                str_pad(edu@geography$tract, 6, "left", pad="0")),
                         do.call("rbind", gender),
                         stringsAsFactors = FALSE)
    colnames(edu_df)[1] <- "GEOID"
    rownames(edu_df)<-1:nrow(edu_df)
    return(edu_df)
  })
  
  hhs_df <- eventReactive(input$GetData, {
    geodata <- geodata(); spatialdata <- spatialdata();
    hhs <- acs.fetch(endyear = 2015, span = 5, geography = geodata, table.number = "B25009", col.names = "pretty")
    progress$inc(.3, detail = paste("Housing data gathered"))
    
    ownership <- list()
    ownership[[1]] <- as.data.frame(hhs@estimate)[c(3:9)]
    ownership[[2]] <- as.data.frame(hhs@estimate)[c(11:17)]
    
    totals <- (ownership[[1]] + ownership[[2]]) %>% rowSums(1)

    
    for (i in 1:2){
      colnames(ownership[[i]]) %<>% strsplit("occupied: ") %>% lapply('[[',2) %>% unlist
      ownership[[i]] <- ownership[[i]] / totals
      ownership[[i]]$ownership <- ifelse(i==1, "Owner", "Renter")
    }
    names <- colnames(ownership[[1]])
    hhs_df <- data.frame(paste0(str_pad(hhs@geography$state, 2, "left", pad="0"), 
                                str_pad(hhs@geography$county, 3, "left", pad="0"), 
                                str_pad(hhs@geography$tract, 6, "left", pad="0")),
                         do.call("rbind", ownership),
                         stringsAsFactors = FALSE)
    colnames(hhs_df)[1] <- "GEOID"; colnames(hhs_df)[-c(1)] <- names
    rownames(hhs_df)<-1:nrow(hhs_df)
    return(hhs_df)
  })
  
  income_merged <- eventReactive(input$GetData, {
    spatialdata <- spatialdata(); income_df2 <- income_df()
    rownames(income_df2)<-1:nrow(income_df2)
    names(income_df2)[c(1,2,19)] <-c("GEOID", "total", "over_150")
    income_df2$percent <- 100*(income_df2$over_150/income_df2$total)
    income_merged<- geo_join(spatialdata, income_df2, "GEOID", "GEOID")
    income_merged[income_merged$ALAND>0,]  
  })
  
  alldata <- eventReactive(input$classification, {
    data <- income_merged()[c(5,11:26)] %>% as.data.frame()
    data[c(3:17)] <- data[c(3:17)] / data$total
    data <- data[-c(2)]
    colnames(data)[2:16] <- c("Less than 10,000", "10,000 to 14,999","15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", "35,000 to 39,999", "40,000 to 44,999", "45,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 to 99,999", "100,000 to 124,999", "125,000 to 149,999", "150,000 to 199,999")
  
    edu_dfs <- split(edu_df[1:8], edu_df$gender)
    colnames(edu_dfs[[1]])[2:8] <- paste("F",colnames(edu_dfs[[1]])[2:8], sep="_")
    colnames(edu_dfs[[2]])[2:8] <- paste("M",colnames(edu_dfs[[2]])[2:8], sep="_")
    edu_df2 <- left_join(edu_dfs[[1]], edu_dfs[[2]])
    
    hhs_df <- split(hhs_df[1:8], hhs_df$ownership)
    colnames(hhs_df[[1]])[2:8] <- paste("O",colnames(hhs_df[[1]])[2:8], sep="_")
    colnames(hhs_df[[2]])[2:8] <- paste("R",colnames(hhs_df[[2]])[2:8], sep="_")
    hhs_df2 <- left_join(hhs_df[[1]], hhs_df[[2]])
    
    alldata <- left_join(left_join(data, edu_df2), hhs_df2)
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
    req(income_merged())
    req(edu_df())
    req(hhs_df())
    progress$close()
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = income_merged(), 
                  fillColor = ~pal()(percent), 
                  color = "#b2aeae",
                  fillOpacity = 0.6, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                  popup = popup(), layerId = income_merged()$GEOID)
  })
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    output$plot=renderPlot({
      p <- NULL
      p <- input$map_shape_click$id
      if(is.null(p)){p=income_merged()$GEOID[1]}
      data <- income_merged()[c(5,11:26)] %>% subset(GEOID == p) %>% as.data.frame()
      data[c(3:17)] <- data[c(3:17)] / data$total
      data <- data[-c(1,2)] %>% t() %>% as.data.frame()
      colnames(data) <- c("Percentage")
      data$bin <- c("Less than 10,000", "10,000 to 14,999","15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", "35,000 to 39,999", "40,000 to 44,999", "45,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 to 99,999", "100,000 to 124,999", "125,000 to 149,999", "150,000 to 199,999") %>% as.factor()
      data$order <- 1:(length(data$bin))
      
      ggplot(data, aes(x=reorder(bin, order), y=Percentage)) + geom_col(fill = "chartreuse4") + 
                 theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + 
                 theme_GR()
    })
    
    output$plot_education=renderPlot({
      p <- NULL
      p <- input$map_shape_click$id
      if(is.null(p)){p=income_merged()$GEOID[1]}
      data <- NULL
      data <- edu_df()[which(edu_df()$GEOID == p),]
      data <- data[-c(1)]
      
      melted <- NULL
      melted <- melt(data, id.vars = "gender")
      
      ggplot(melted, aes(x=variable, y=value, fill = gender)) + geom_col(position = "dodge") + 
                 theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
                 scale_fill_manual(values = c("pink", "steelblue1")) + theme_GR()
    })
    
    output$plot_housing=renderPlot({
      p <- NULL
      p <- input$map_shape_click$id
      if(is.null(p)){p=income_merged()$GEOID[1]}
      data <- hhs_df()[unique(which(hhs_df()$GEOID == p)),]
      data <- data[-c(1)]
      
      melted <- melt(data, id.vars = "ownership")
      
      ggplot(melted, aes(x=variable, y=value, fill = ownership)) + geom_col(position = "dodge") + 
                 theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
                 scale_fill_manual(values = c("orange", "purple")) + theme_GR()
    })
  })
  
  session$onSessionEnded(stopApp)  
   
})