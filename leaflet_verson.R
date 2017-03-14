library(XML)
library(RCurl)
library(stringr)
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(dplyr)
library(RJSONIO)
library(acs)
library(reshape2)
library(magrittr)
library(animation)
library(gganimate)
library(tigris)
library(rgdal)    
library(sp)       
library(leaflet)  


# List of Counties around South Bend
cntyList=c("St. Joseph", "Elkhart","CassMI")

spatialdataIN <- tracts(state="IN", county=c("St. Joseph","Elkhart"), cb=TRUE)
spatialdataMI <- tracts(state="MI", county=c("Cass","Berrien"), cb=TRUE)
spatialSB <- rbind_tigris(spatialdataIN, spatialdataMI)

geoIN <- geo.make(state="IN", county=c("St. Joseph","Elkhart"), tract="*")
geoMI <- geo.make(state="MI", county=c("Cass","Berrien"), tract="*")
SBarea <- geoIN + geoMI

income <-acs.fetch(endyear = 2012, span = 5, geography = SBarea, table.number = "B19001", col.names = "pretty")

#### Split ####
incomeIN <-acs.fetch(endyear = 2012, span = 5, geography = geoIN, table.number = "B19001", col.names = "pretty")
incomeMI <-acs.fetch(endyear = 2012, span = 5, geography = geoMI, table.number = "B19001", col.names = "pretty")

incomeIN_df <- data.frame(paste0(str_pad(incomeIN@geography$state, 2, "left", pad="0"), 
                               str_pad(incomeIN@geography$county, 3, "left", pad="0"), 
                               str_pad(incomeIN@geography$tract, 6, "left", pad="0")), 
                          incomeIN@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                               stringsAsFactors = FALSE)

incomeMI_df <- data.frame(paste0(str_pad(incomeMI@geography$state, 2, "left", pad="0"), 
                               str_pad(incomeMI@geography$county, 3, "left", pad="0"), 
                               str_pad(incomeMI@geography$tract, 6, "left", pad="0")), 
                        incomeMI@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

incomeIN_df <- select(incomeIN_df, 1:3)
rownames(incomeIN_df)<-1:nrow(incomeIN_df)
names(incomeIN_df)<-c("GEOID", "total", "over_200")
incomeIN_df$percent <- 100*(incomeIN_df$over_200/incomeIN_df$total)

incomeMI_df <- select(incomeMI_df, 1:3)
rownames(incomeMI_df)<-1:nrow(incomeMI_df)
names(incomeMI_df)<-c("GEOID", "total", "over_200")
incomeMI_df$percent <- 100*(incomeMI_df$over_200/incomeMI_df$total)


incomeIN_merged<- geo_join(spatialdataIN, incomeIN_df, "GEOID", "GEOID")
incomeIN_merged <- incomeIN_merged[incomeIN_merged$ALAND>0,]
incomeMI_merged<- geo_join(spatialdataMI, incomeMI_df, "GEOID", "GEOID")
incomeMI_merged <- incomeMI_merged[incomeMI_merged$ALAND>0,]




popup <- paste0("GEOID: ", incomeIN_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(incomeIN_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = incomeIN_merged$percent
)

mapIN <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = incomeIN_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = incomeIN_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 

mapIN


popup <- paste0("GEOID: ", incomeMI_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(incomeMI_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = incomeMI_merged$percent
)

mapMI <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = incomeMI_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = incomeMI_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 

mapMI



#### Merged ####
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
income_merged<- geo_join(spatialSB, income_df, "GEOID", "GEOID")
income_merged <- income_merged[income_merged$ALAND>0,]

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)

map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 

map
