#######################################
### BULLET CHART
#######################################
bg.data <- read.xlsx("Data/BulletData.xlsx", sheetName = "Sheet1", rowIndex = 1:6)
names(bg.data)
library(RSvgDevice)
  svg("bullets.svg", height = 5, width = 5)
  gg <- ggplot(bg.data) 
  gg <- gg + geom_bar(aes(Measure, High),  fill="#7C94A1", stat="identity", width=0.5, alpha=0.5) 
  gg <- gg + geom_bar(aes(Measure, Mean),  fill="#4C81A1", stat="identity", width=0.5, alpha=0.5) 
  gg <- gg + geom_bar(aes(Measure, Low),   fill="#284455", stat="identity", width=0.5, alpha=0.5) 
  gg <- gg + geom_bar(aes(Measure, X2013), fill="black",  stat="identity", width=0.2) 
  gg <- gg + geom_errorbar(aes(y=Target, x=Measure, ymin=Target, ymax=Target), color="#233B4A", width=0.45) 
  gg <- gg + geom_point(aes(Measure, Target), colour="#233B4A", size=2.5) 
  gg <- gg + coord_flip()
  gg <- gg + theme(axis.text.x=element_blank(),
                   axis.title.x=element_blank(),
                   axis.line.y=element_blank(), 
                   axis.text.y=element_blank(), 
                   axis.ticks.y=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="none",
                   panel.background=element_blank(), 
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())
  gg 
  dev.off()
  
  
  
  
  
#######################################
### MAPPING
#######################################

  ## VIOLENCE MAPPING
  library(rgdal)
  violence <- read.csv("Data/conflicts.csv", header = TRUE)
  names(violence)
  conflicts <- violence %>% 
    dplyr::group_by(conflict_new_id, latitude, longitude, year, type_of_violence) %>%
    dplyr::summarise(best_est = sum(best_est))
  
  # Read in map JSON
  countries <- readOGR("Data/geo-countries-master/data/countries.geojson", "OGRGeoJSON")
  names(countries)
  summary(countries)
  
  # Subset out the African countries
  afCountries <- subset(countries, countries$ISO_A3 %in% c(
    "DZA", "AGO", "BDA", "BEN","BWA","BFA","CMR","CAF","TCD","COG","CIV","COD", "COM",
    "DJA", "DZA", "EGY", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", 
    "LSO", "LBR", "LBY", "MDG", "MRT", "MUS", "MAR", "MWI", "MLI", "SDN", "SSD",
    "MOZ", "NAM", "NER", "NGA", "RWA", "SEN", "SHN", "SLE", "STP", "SOM", "SWZ", "ZAF", "TZA",
    "TGO", "TUN", "UGA", "ZAF", "ZMB", "ZWE"))

  
  m1 <- leaflet(data = afCountries) %>% addPolygons(fillColor = gray.colors(10, alpha = NULL), stroke = FALSE) %>% addProviderTiles("CartoDB.DarkMatter")
  m2 <- leaflet(data = afCountries) %>% addPolygons(fillColor = gray.colors(10, alpha = NULL), stroke = FALSE) 
  
  
  
  ## ELEPHANT POPULATIONS
  
  elephMapData <- read.xlsx("Data/elephByCountry_master.xlsx", sheetName = "Sheet1")
  names(elephMapData)
  
  ## Merge data and map files
  # Make a copy of the data frame and subset columns that are constant across all years
  df <- elephMapData[, c("region", "subregionid", "country", "ISO2", "ISO3", "cap.lat", "cap.long")]
  df <- unique(df)
  
  # 'cast' the data into a tidy form
  library("reshape2")
  dfe <- dcast(elephMapData, ISO3 ~ year, value.var = "Diff.from.expected")
  colnames(dfe) <- c("ISO3", "2002_diff.from.expected", "2007_diff.from.expected", "2013_diff.from.expected")

  dfe1 <- dcast(elephMapData, ISO3 ~ year, value.var = "Change.by.year")
  colnames(dfe1) <- c("ISO3", "2002_Change.by.year", "2007_Change.by.year", "2013_Change.by.year")
  
  dfe2 <- dcast(elephMapData, ISO3 ~ year, value.var = "Elephant.range")
  colnames(dfe2) <- c("ISO3", "2002_Elephant.range", "2007_Elephant.range", "2013_Elephant.range")
  
  dfe3 <- dcast(elephMapData, ISO3 ~ year, value.var = "Definite.Probable")
  colnames(dfe3) <- c("ISO3", "2002_Definite.Probable", "2007_Definite.Probable", "2013_Definite.Probable")
  
  dfe6 <- dcast(elephMapData, ISO3 ~ year, value.var = "PIKE.regional")
  colnames(dfe6) <- c("ISO3", "2002_PIKE.regional", "2007_PIKE.regional", "2013_PIKE.regional")
  
  dfe5 <- dcast(elephMapData, ISO3 ~ year, value.var = "percent.ill")
  colnames(dfe5) <- c("ISO3", "2002_percent.ill", "2007_percent.ill", "2013_percent.ill")
  
  # merge these into the spatial data
  afCountries@data <- merge(afCountries@data, dfe, by.x = "ISO_A3", by.y = "ISO3")
  afCountries@data <- merge(afCountries@data, dfe1, by.x = "ISO_A3", by.y = "ISO3")
  afCountries@data <- merge(afCountries@data, dfe2, by.x = "ISO_A3", by.y = "ISO3")
  afCountries@data <- merge(afCountries@data, dfe3, by.x = "ISO_A3", by.y = "ISO3")
  afCountries@data <- merge(afCountries@data, dfe6, by.x = "ISO_A3", by.y = "ISO3")
  afCountries@data <- merge(afCountries@data, dfe5, by.x = "ISO_A3", by.y = "ISO3")
  
  # merge the 'constants' back in
  afCountries@data <- merge(afCountries@data, df, by.x = "ISO_A3", by.y = "ISO3")
  str(afCountries@data)
  # Create a continuous palette function
  pal <- colorNumeric(
    palette = "BuGn",
    domain = elephMapData$PIKE.regional
  )
  
  
  map02 <- leaflet(afCountries, width = 680, height = 680) %>%
    
  

###############################################
## Tanh transformation of data for radial data viz
###############################################
names(a.coVarsTrans.more$imputations$imp1)
  str(a.coVarsTrans.more$imputations$imp1)
write.amelia(a.coVarsTrans.more, separate = FALSE, file.stem = "outdata", orig.data = FALSE)
a.coVarsTrans.tanh <- read.csv("outdata.csv")
a.coVarsTrans.ids <- a.coVarsTrans.tanh[,c(1:10,52)]
a.coVarsTrans.x <- a.coVarsTrans.tanh[, c(11:51)]
a.coVars.tanh <- tanh(a.coVarsTrans.x)
a.coVars.tanh <- cbind(a.coVarsTrans.ids, a.coVars.tanh)
names(a.coVars.tanh)
write.csv(a.coVars.tanh, "a.coVars.tanh.csv")

GNI <- 

a.coVarsTrans.tanh <- lapply(a.coVarsTrans.tanh[,9:50], 1, FUN = tanh)

for(i in 1:a.coVarsTrans.more$imputations) {
   ols.out <- lm(tariff ~ polity + pop + gdp.pc, data = a.out$imputations[[i]])
  + b.out <- rbind(b.out, ols.out$coef)
  + se.out <- rbind(se.out, coef(summary(ols.out))[,2])
  + }
    