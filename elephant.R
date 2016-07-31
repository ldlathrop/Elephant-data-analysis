library(xlsx)
library(Amelia)
library(ggplot2)
library(ggthemes)
library(grDevices)
library(colorRamps)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(sp)
library(leaflet)
library(magrittr)
library(raster)
library(splancs)
library(maps)
library(lattice)
library(latticeExtra)
library(colorspace)
library(taRifx.geo)
library(rgeos)
library(classInt)
library(tidyr)
library(lubridate)
library(sparkline)
library(htmlwidgets)
library(MCMCpack)
library(bayesm)
library(RSGHB)
library(rjags)
library(runjags)
library(psych)
library(blme)


require(devtools)
install_url("https://cran.r-project.org/src/contrib/Archive/taRifx.geo/taRifx.geo_1.0.6.tar.gz")

### READ IN THE DATA FILES
elephantDataAllYrs <- read.xlsx("Elephant database all years.xlsx", header = TRUE, 
                                sheetName = "Sheet2", rowIndex = 1:26)
names(elephantDataAllYrs)
tail(elephantDataAllYrs)
mikeCarcasses <- read.xlsx("Carcasses reported to MIKE by site and year.xlsx", header = TRUE,
                           sheetName = "Reported")
names(mikeCarcasses)
citesTradeData <- read.csv("CITES-Trade data.csv", header = TRUE)

mikeSiteBoundaries <- read.csv("MIKE site boundaries.csv", header = TRUE)

nonStateConflict <- read.xlsx("Data/ucdp-non-state-conflict-Africa-2015.xlsx", sheetName = "Africa")
names(nonStateConflict)
nonStateConflictByCountry <- nonStateConflict %>%
  dplyr::select(SideA, SideB, Year, BestFatalityEstimate, Location) %>%
  dplyr::group_by(Location, Year) %>%
  dplyr::summarise(BestFatalityEstimate = sum(BestFatalityEstimate))
nonStateConflictByCountry <- data.frame(nonStateConflictByCountry)
write.csv(nonStateConflictByCountry, "Data/nonStateByCountry.csv")

### EXPLORE THE DATA
## Explore the elephant data
library(dplyr)
elephantDataSubset <- elephantDataAllYrs[, c(2:4,12:15)] %>%
  names(elephantDataSubset)
write.csv(elephantDataSubset, "elephDataSub.csv")
elephantDataSubset <-  group_by(elephantDataSubset, Region)
names(elephantDataSubset)
tail(elephantDataSubset)
elephantDataSubset$RegionID <- as.factor(elephantDataSubset$RegionID)
str(elephantDataSubset)
levels(elephantDataSubset$Region)

fill1 <- rgb(209/255, 242/255, 45/255, alpha = 0.8)
fill2 <- rgb(94/255, 41/255, 242/255, alpha = 0.8)
fill3 <- rgb(242/255, 39/255, 99/255, alpha = 0.8)
fill4 <- rgb(26/255, 202/255, 255/255, alpha = 0.8)
fill5 <- rgb(255/255, 165/255, 47/255, alpha = 0.8)
cols <- c(fill1, fill2, fill3, fill4, fill5)
fill4 <- c(fill1, fill2, fill3, fill4)

# Example
## ggplot(mydata, aes(Var1, Var2)) + geom_point() + facet_grid(~ Variety)
elephantDataSubset$Year <- paste(elephantDataSubset$Year, "01", "01", sep = "-")
elephantDataSubset$Year <- ymd(elephantDataSubset$Year)
g <- ggplot(data = elephantDataSubset, mapping = aes(x=Year, y=Definite.Probable,
                                                     group = Region, color=as.factor(Region))) +
  geom_line() 
g <- g +  geom_point(data = elephantDataSubset, aes(x=Year, y=Projected.growth_Definite.Probable, 
                group = Region, color=as.factor(Region))) 
g <- g + scale_color_manual(values = fill) 
g <- g + facet_grid(facets = Region ~ .)
g <- g + theme_bw()
g <- g + theme()
g

g1 <- ggplot(data = mikeCarcasses, aes(x=year, y=totcarc, colour=as.factor(region))) +
  geom_point()
g1 <- g1 + geom_point(data = mikeCarcasses, aes(x=year, y=illegal, colour=as.factor(region))) +
  stat_smooth(method = lm)
g1 <- g1 + scale_colour_manual(values = fill4)
g1 <- g1 + theme_bw()
g1


g6 <- ggplot(elephantDataSubset, aes(factor(Year), Definite.Probable, group = 1, fill = Region, size=Definite.Probable))
g6 <- g6 + geom_point()
g6 <- g6 + geom_point(data=elephantDataSubset, aes(factor(Year), Projected.growth...Definite.Probable, 
                                                   group=1, fill=Region, size=Projected.growth...Definite.Probable))
g6 <- g6 + scale_fill_manual(values = c("#D1F22DCC", "#5E29F2CC", "#F22763CC", "#1ACAFFCC", "#FFA52FCC"), name = "Region", 
                              labels = c("Central Africa", "Eastern Africa", "Southern Africa", "Totals", "West Africa")) +
  xlab("Year") + ylab("Actual vs. Projected") + ggtitle("Actual counts vs. Projected counts") 
g6
## Try ggplot with ggfortify
elephantTimeSeries <- as.matrix(elephantDataSubset[, c(5,2,8,7)])
autoplot(elephantTimeSeries)
library(plotly)
p <- plot_ly(elephantDataSubset, x = Year, y = Definite.Probable, color = Region,
             mode = "lines") %>%
  add_trace(p, y=Projected.growth...Definite.Probable, color = Region,
            mode = "lines"
            ) %>% layout(p, title="", 
                         xaxis=list(title="Year", range=c(1995:2015)),
                         yaxis=list(title="Actual vs. Projected", range=c(0,1000000)),
                         showlegend = TRUE)
p

library(WVPlots)

## Try with base R graphics
elephantDataSubsetGrouped <-  group_by(elephantDataSubset, Region)
png("elephant.png", height = 1020, width = 630)
par(mfrow=c(5, 1), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(elephantDataSubsetGrouped, {
  plot(Year, Definite.Probable, xlab = "", ylab = "Actual vs. Projected", type = "l")
  with(elephantDataSubsetGrouped, lines(Year, Projected.growth...Definite.Probable, type = "l", lty=3))
  legend("topright", legend = elephantDataSubsetGrouped$Region, lty = "solid", col = c("#D1F22DCC", "#5E29F2CC", "#F22763CC", "#1ACAFFCC", "#FFA52FCC"))
})
dev.off()

library(lattice)
library(latticeExtra)
png("defproj.png", height = 640, width = 480)
def <- xyplot(Definite.Probable + Projected.growth...Definite.Probable ~ Year | RegionID,
       data = elephantDataSubset, type = "l", lty = 1, lwd = 2, 
       colors = c(rgb(209/255, 242/255, 45/255, alpha = 0.8), 
                rgb(94/255, 41/255, 242/255, alpha = 0.8)),
       auto.key = list(space = "right", cex = 0.5,), layout = c(1, 5), ylim = c(0,1000000),
       xlab = "Year", ylab = "Actual count vs. Projected growth",
       scales = list(tick.number = 5, at = c(100000, 300000, 500000, 700000,900000), 
                     cex = 0.5, rot = 45, abbreviate = FALSE))
        
def
dev.off()

png("defproj.png", height = 640, width = 480)
def <- dotplot(Definite + Projected.growth.Definite ~ Year | Region, data = elephantDataSubset, type = "count",
        auto.key = list(space = "right"), layout = c(1, 5), ylim = c(200000,1000000),
        xlab = "Year", ylab = "Definite count & Projected growth", options(scipen = 7),
        scales = list(tick.number = 5, at = c(200000, 400000, 600000, 800000, 1000000), 
                      cex = 0.5, rot = 45, abbreviate = FALSE))
def
dev.off()

## Explore the 'carcasses' dataset
library(dplyr)
names(mikeCarcasses)
totcarcByRegion <- data.frame(xtabs(totcarc ~ region, data = mikeCarcasses))
illegalByRegion <- data.frame(xtabs(illegal ~ region, data = mikeCarcasses))

totcarcByYear <- data.frame(xtabs(totcarc ~ year, data = mikeCarcasses))
illegalByYear <- data.frame(xtabs(illegal ~ year, data = mikeCarcasses))

mikeSubset <- subset(mikeCarcasses, select=c(region, region.lat, region.long,
                                             X3.digit.ccode:country.long,
                                             year:illegal))
mikeSubset <- mutate(mikeSubset, 
                        illegOfTotal = (illegal/totcarc))

mikeByReg <- mikeSubset %>% 
  dplyr::group_by(region, year) %>%
  dplyr::summarise(sumTot = sum(totcarc),
                   sumIll = sum(illegal),
                   meanIll = mean(illegal), 
                   sdIll = sd(illegal),
                   meanIllOfTot = mean(illegOfTotal),
                   sdIllOfTot = sd(illegOfTotal))
mikeByReg <- data.frame()

mikeByCountry <- mikeSubset %>% 
  dplyr::group_by(country, year) %>%
  dplyr::summarise(sumTot = sum(totcarc),
                   sumIll = sum(illegal),
                   meanIll = mean(illegal), 
                   sdIll = sd(illegal),
                   meanIllOfTot = mean(illegOfTotal),
                   sdIllOfTot = sd(illegOfTotal))


mikeByCountryYear <- mikeSubset %>% 
  dplyr::group_by(country, year) %>%
  dplyr::summarise(sumTot = sum(totcarc),
                   sumIll = sum(illegal))

mikeByCountryYear <- data.frame(mikeByCountryYear)
write.csv(mikeByCountryYear, "mikeCountryYear.csv")

#######################################
### SLOPE CHART
#######################################

slopeVals <- read.xlsx("Elephant database for slope.xlsx", sheetName = "Sheet2")
slope <- ggplot(slopeVals, aes(x=Year, y=Definite.Probable, colour=as.factor(Region), group=1)) +
  geom_line(colour=cols) + geom_point()
slope <- slope + geom_line(aes(x=Year, y=Projected.growth_Definite.Probable, colour=as.factor(Region), group=1),
                           linetype="dashed", colour=cols) + geom_point()
slope



