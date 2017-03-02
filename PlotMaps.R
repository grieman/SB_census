library(XML)
library(RCurl)
library(stringr)
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(plyr)
library(RJSONIO)
library(acs)

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
