library(stringr)
library(zipcode)
library(pdftools)

download.file("https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf", "countynames.pdf", mode = "wb")
stateCntyCodes  <- pdf_text("countynames.pdf")

#parse PDF file to get IN data
tabl <- list()
for (i in 10:11){
  temp <- stateCntyCodes[i] %>% strsplit("\r\n") %>% unlist() 
  temp <- temp[-c(1,40)] %>% strsplit("I.") %>% unlist() %>% strsplit("   ") %>% unlist() %>% 
    strsplit("  ") %>% unlist()
  
  temp %<>% subset(nchar(temp) > 1) %>% strsplit(" ") %>% unlist() %>% paste(collapse=" ")
  
  allsplit <- temp %>% strsplit(" ") %>% unlist()
  statecodes <- subset(allsplit, allsplit %in% c("17","18","19"))
  countycodes <- subset(allsplit, nchar(allsplit) == 3 & allsplit != "Jay"& allsplit != "St."& allsplit != "Des")
  
  counties <- temp %>% strsplit(" .. ... ") %>% unlist()
  counties[1] %<>% strsplit(" ") %>% lapply("[[",3) %>% unlist()
  
  tabl[[i]] <- cbind(statecodes, countycodes, counties) %>% as.data.frame
}
stateCntyCodes <- do.call("rbind", tabl) %>% subset(statecodes == "18")
colnames(stateCntyCodes) <- c("fipstate","fipscty","ctyname")
stateCntyCodes <- sapply(stateCntyCodes, as.character)
stateCntyCodes <- rbind(stateCntyCodes, c("26","027","Cass")) %>% as.data.frame()
stateCntyCodes <- sapply(stateCntyCodes, as.factor) %>% as.data.frame()





zipCnty = read.table("zcta_county_rel_10.txt", sep = ",", colClasses = c("character"), 
                     header = TRUE)
data(zipcode)

zipMap = merge(zipCnty[, c("ZCTA5", "STATE", "COUNTY")], 
               zipcode[, c("zip","city")], by.x = "ZCTA5", by.y = "zip")

zipMap = merge(zipMap, stateCntyCodes, by.x = c("STATE", "COUNTY"), by.y = c("fipstate", 
                                                                              "fipscty"))
save(zipMap, file = "zipCityCountyStateMap.Rda")
