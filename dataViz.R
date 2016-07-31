#######################################
### BULLET CHART
#######################################
bg.data <- read.xlsx("Data/BulletData.xlsx", sheetName = "Sheet1", rowIndex = 1:6)
names(bg.data)

bullet.graph <- function(bg.data){
  
  # compute max and half for the ticks and labels
  max.bg <- max(bg.data$High)
  low.bg <- max.bg / 3
  mid.bg <- low.bg*2
  
  svg("bullets.svg", height = 680, width = 680)
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
  library(rgdal)
  violence <- read.csv("Data/conflicts.csv", header = TRUE)
  names(violence)
  conflicts <- violence %>% 
    dplyr::group_by(conflict_new_id, latitude, longitude, year, type_of_violence) %>%
    dplyr::summarise(best_est = sum(best_est))
  countries <- readOGR("Data/geo-countries-master/data/countries.geojson", "OGRGeoJSON")
  names(countries)
  summary(countries)
  
  afCountries <- subset(countries, countries$ISO_A3 %in% c(
    "AGO", "BDA", "BEN","BWA","BFA","CMR","CAF","TCD","COG","CIV","COD", "COM",
    "DJA", "DZA", "EGY", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", 
    "LSO", "LBR", "LBY", "MDG", "MRT", "MUS", "MAR", "MWI", "MLI", "SDN", "SSD",
    "MOZ", "NAM", "NER", "NGA", "RWA", "SEN", "SHN", "SLE", "STP", "SOM", "SWZ", "ZAF", "TZA",
    "TGO", "TUN", "UGA", "ZAF", "ZMB", "ZWE"))
  
  quartz()
  m1 <- leaflet(data = countries) %>% setMaxBounds(lng1=-18.2,lat1=25.6, lng2=45.4, lat2=-35.8) %>% addPolygons(fillColor = gray.colors(10, alpha = NULL), stroke = FALSE) %>% addProviderTiles("CartoDB.DarkMatter")
  
  m2 <- leaflet(data = afCountries) %>% addPolygons(fillColor = gray.colors(6, alpha = NULL), stroke = FALSE) 
  
  m3 <- leaflet(data = countries) %>% setMaxBounds(lng1=-18.2,lat1=25.6, lng2=45.4, lat2=-35.8) %>% addPolygons(stroke = FALSE) 
  
  