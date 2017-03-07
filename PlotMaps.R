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

#List of Counties around South Bend
cntyList=c("St. Joseph","Cass")

# get list of zips in the South Bend area
zips=zipMap[zipMap$ctyname %in% cntyList,]

# remove duplicates
zips2=zips[!duplicated(zips$ZCTA5),]
head(zips2)

zipShp <- rgdal::readOGR("cb_2015_us_zcta510_500k//cb_2015_us_zcta510_500k.shp")
zipShp2 <- zipShp[zipShp$ZCTA5CE10 %in% zips2$ZCTA5,]
zipShp2 <- fortify(zipShp2,region="ZCTA5CE10")


x <- get_googlemap(center="south bend",maptype=c("roadmap"))

map <- ggmap(x)
map + geom_polygon(data=zipShp2, aes(x=long, y=lat, group=id), fill = "blue", color = "black", alpha = .2)

api.key.install("4f56e4ff2ade70c0c3cc47779dd337334e6703d6")

household_income <- acs.fetch(2015, geography = geo.make(zip.code = zips2$ZCTA5), table.number = "B25119")
c("B25021","B25037","B25039","B25119","B25077")


data <- cbind(household_income@geography[,2], household_income@estimate %>% as.data.frame()) %>% as.data.frame(); colnames(data) <- c("zip", "est1","est2","est3")
data[c(2:4)] %<>% sapply(as.numeric); data[c(1)] %<>% sapply(as.character)
zip3 <- left_join(zipShp2, data, by = c("id"="zip")) 
zip3$est1 %<>% as.character() %>% as.numeric %>% cut(7)

map + geom_polygon(data=zip3, aes(x=long, y=lat, group=id, fill = est1), alpha = .4) + 
  scale_fill_brewer(type="seq", palette="YlGnBu", direction = 1)




devtools::install_github("dgrtwo/gganimate")
library("gganimate")

#https://www.imagemagick.org/script/download.php

melted <- melt(data); colnames(melted) <- c("zips", "year", "value")
zip4 <- left_join(zipShp2, melted, by = c("id"="zips")) 
zip4$value %<>% cut(7)



map2 <- ggplot(data=zip4, aes(x=long, y=lat, group=id, fill = value, frame = year), alpha = .4) + geom_polygon() +
  scale_fill_brewer(type="seq", palette="YlGnBu", direction = 1)
#ani.options(convert = "C:\\Program Files\\ImageMagick-7.0.5-Q16\\convert.exe")
#magickPath <- shortPathName("C:\\Program Files\\ImageMagick-7.0.3-Q16\\magick.exe")
#ani.options(convert=magickPath)
gganimate(map2)


installr:::install.ImageMagick() #install convert
