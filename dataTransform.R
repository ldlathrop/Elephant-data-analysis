#######################################
### TRANSFORM DATA
#######################################
library(car)
coVarsTrans <- coVars

lambda1 <- coef(powerTransform(coVarsTrans$NGDPD))
coVarsTrans$NGDPD <- bcPower(coVarsTrans$NGDPD, lambda1)

lambda2 <- coef(powerTransform(coVarsTrans$NGDPDPC))
coVarsTrans$NGDPDPC <- bcPower(coVarsTrans$NGDPDPC, lambda2)

lambda3 <- coef(powerTransform(coVarsTrans$PCPI))
coVarsTrans$PCPI <- bcPower(coVarsTrans$PCPI, lambda3)

lambda4 <- coef(powerTransform(coVarsTrans$GGX_NGDP))
coVarsTrans$GGX_NGDP <- bcPower(coVarsTrans$GGX_NGDP, lambda4)

coVarsTrans$HDI <- logit(coVarsTrans$HDI)

lambda6 <- coef(powerTransform(coVarsTrans$GNI))
coVarsTrans$GNI <- bcPower(coVarsTrans$GNI, lambda6)

coVarsTrans$Resource.Depletion <- logit(coVarsTrans$Resource.Depletion)

lambda7 <- coef(powerTransform(coVarsTrans$Total.pop))
coVarsTrans$Total.pop <- bcPower(coVarsTrans$Total.pop, lambda7)

lambda8 <- coef(powerTransform(coVarsTrans$Pop.MultiDim.Povert))
coVarsTrans$Pop.MultiDim.Povert <- bcPower(coVarsTrans$Pop.MultiDim.Povert, lambda8)

lambda9 <- coef(powerTransform(coVarsTrans$International.Dev.Aid))
coVarsTrans$International.Dev.Aid <- bcPower(coVarsTrans$International.Dev.Aid, lambda9)

lambda10 <- coef(powerTransform(coVarsTrans$Corruption.Perception.Index))
coVarsTrans$Corruption.Perception.Index <- bcPower(coVarsTrans$Corruption.Perception.Index, lambda10)

head(coVars)
# Run Amelia to impute missing values
set.seed(1234)
a.coVarsTrans.time08 <- amelia(coVarsTrans, idvars = c("region", "subregionid", "ISO2",
                                                       "ISO3", "cap.lat", "cap.long"), 
                               ts = "year", cs = "country", polytime = 1, 
                               intercs = TRUE, p2s = 2, empri = .08*nrow(coVarsTrans),
                              parallel = "multicore", ncpus = 3)
summary(a.coVarsTrans.time08)

write.amelia(obj=a.coVarsTrans.time08, file.stem = "outdata08")

# Diagnostic plots

png("hdiImputations.png", width = 480, height = 800)
par(mfrow=c(5,1), mar=c(0,0,0,0))
hist(a.coVarsTrans.time08$imputations[[1]]$HDI, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[2]]$HDI, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[3]]$HDI, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[4]]$HDI, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[5]]$HDI, col="coral1", breaks=30, border="white", xlab="")
dev.off()

png("multiDimImputations.png", width = 480, height = 800)
par(mfrow=c(5,1), mar=c(0,0,0,0))
hist(a.coVarsTrans.time08$imputations[[1]]$Pop.MultiDim.Povert, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[2]]$Pop.MultiDim.Povert, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[3]]$Pop.MultiDim.Povert, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[4]]$Pop.MultiDim.Povert, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[5]]$Pop.MultiDim.Povert, col="coral1", breaks=30, border="white", xlab="")
dev.off()

png("cpiImputations.png", width = 480, height = 800)
par(mfrow=c(5,1), mar=c(0,0,0,0))
hist(a.coVarsTrans.time08$imputations[[1]]$Corruption.Perception.Index, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[2]]$Corruption.Perception.Index, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[3]]$Corruption.Perception.Index, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[4]]$Corruption.Perception.Index, col="coral1", breaks=30, border="white", xlab="")
hist(a.coVarsTrans.time08$imputations[[5]]$Corruption.Perception.Index, col="coral1", breaks=30, border="white", xlab="")
dev.off()

par(mfrow=c(1,1))
overimpute(a.coVarsTrans.time08, var = "HDI")
overimpute(a.coVarsTrans.time08, var = "Adult.literacy")
overimpute(a.coVarsTrans.time08, var = "Primary.ed.enrollment")
overimpute(a.coVarsTrans.time08, var = "Resource.Depletion")

names(coVarsTrans)
png("diag1.png", height = 680, width = 680)
plot(a.coVarsTrans.time08, which.vars = 19:24)
dev.off()


png("diag2.png", height = 680, width = 680)
plot(a.coVarsTrans.time08, which.vars = 26:31)
dev.off()

png("diag3.png", height = 680, width = 680)
plot(a.coVarsTrans.time08, which.vars = 36:40)
dev.off()

png("diag4.png", height = 680, width = 680)
plot(a.coVarsTrans.time08, which.vars = 45:50)
dev.off()

tscsPlot(a.coVarsTrans.time08, cs = "Angola", main = "Angola (0.08, with time settings)",
         var = "HDI")
tscsPlot(a.coVarsTrans.time08, cs = "Angola", main = "Angola (0.08, with time settings)",
         var = "GNI")
tscsPlot(a.coVarsTrans.time08, cs = "Angola", main = "Angola (0.08, with time settings)",
         var = "Resource.Depletion")

# Additional run to be combined with initial run
a.coVarsTrans.time08.more <- amelia(coVarsTrans, idvars = c("region", "subregionid", "ISO2",
                                                            "ISO3", "cap.lat", "cap.long"), 
                                    m=15, ts = "year", cs = "country", polytime = 1, 
                                    intercs = TRUE, p2s = 2, empri = .08*nrow(coVarsTrans),
                                    parallel = "multicore", ncpus = 3)
summary(a.coVarsTrans.time08.more)
a.coVarsTrans.more <- ameliabind(a.coVarsTrans.time08, a.coVarsTrans.time08.more)
write.amelia(obj=a.coVarsTrans.more, file.stem = "coVarsTrans", separate = FALSE, orig.data = FALSE)
summary(a.coVarsTrans.more)

## Save for question
save(a.coVarsTrans.more, file = "coVarsTrans.RData")

coVarsTrans.ScaleZ <- read.csv("coVarsTrans.csv", header = TRUE)
names(coVarsTrans.ScaleZ)
head(coVarsTrans.ScaleZ)
coVarsNoTrans <- coVarsTrans.ScaleZ[, c(1:9, 43:45, 52)]
coVarsTrans.ScaleZ <- coVarsTrans.ScaleZ[,c(10:42, 45:51)]

coVarsTrans.ScaleZ <- data.frame(scale(coVarsTrans.ScaleZ, center = TRUE, scale = TRUE))

coVarsTrans.ScaleZ$NGDPD.z <- coVarsTrans.ScaleZ$NGDPD - mean(coVarsTrans.ScaleZ$NGDPD)/sd(coVarsTrans.ScaleZ$NGDPD)
coVarsTrans.ScaleZ$NGDPDPC.z <- coVarsTrans.ScaleZ$NGDPDPC - mean(coVarsTrans.ScaleZ$NGDPDPC)/sd(coVarsTrans.ScaleZ$NGDPDPC)
coVarsTrans.ScaleZ$PCPI.z <- coVarsTrans.ScaleZ$PCPI - mean(coVarsTrans.ScaleZ$PCPI)/sd(coVarsTrans.ScaleZ$PCPI)
coVarsTrans.ScaleZ$HDI.z <- coVarsTrans.ScaleZ$HDI - mean(coVarsTrans.ScaleZ$HDI)/sd(coVarsTrans.ScaleZ$HDI)
coVarsTrans.ScaleZ$GNI.z <- coVarsTrans.ScaleZ$GNI - mean(coVarsTrans.ScaleZ$GNI)/sd(coVarsTrans.ScaleZ$GNI)
coVarsTrans.ScaleZ$Resource.Depletion.z <- coVarsTrans.ScaleZ$Resource.Depletion - mean(coVarsTrans.ScaleZ$Resource.Depletion)/sd(coVarsTrans.ScaleZ$Resource.Depletion)
coVarsTrans.ScaleZ$Adult.literacy.z <- coVarsTrans.ScaleZ$Adult.literacy - mean(coVarsTrans.ScaleZ$Adult.literacy)/sd(coVarsTrans.ScaleZ$Adult.literacy)
coVarsTrans.ScaleZ$Primary.ed.enrollment.z <- coVarsTrans.ScaleZ$Primary.ed.enrollment - mean(coVarsTrans.ScaleZ$Primary.ed.enrollment)/sd(coVarsTrans.ScaleZ$Primary.ed.enrollment)
coVarsTrans.ScaleZ$Deprivation.Intensity.z <- coVarsTrans.ScaleZ$Deprivation.Intensity - mean(coVarsTrans.ScaleZ$Deprivation.Intensity)/sd(coVarsTrans.ScaleZ$Deprivation.Intensity)
coVarsTrans.ScaleZ$Pop.Below.National.Poverty.z <- coVarsTrans.ScaleZ$Pop.Below.National.Poverty - mean(coVarsTrans.ScaleZ$Pop.Below.National.Poverty)/sd(coVarsTrans.ScaleZ$Pop.Below.National.Poverty)

coVarsTrans.ScaleZ$PPP.125.day.z <- coVarsTrans.ScaleZ$PPP.125.day - mean(coVarsTrans.ScaleZ$PPP.125.day)/sd(coVarsTrans.ScaleZ$PPP.125.day)
coVarsTrans.ScaleZ$Corruption.Perception.Index.z <- coVarsTrans.ScaleZ$Corruption.Perception.Index - mean(coVarsTrans.ScaleZ$Corruption.Perception.Index)/sd(coVarsTrans.ScaleZ$Corruption.Perception.Index)
coVarsTrans.ScaleZ$Voice.Accountability.z <- coVarsTrans.ScaleZ$Voice.Accountability - mean(coVarsTrans.ScaleZ$Voice.Accountability)/sd(coVarsTrans.ScaleZ$Voice.Accountability)
coVarsTrans.ScaleZ$Political.Stability.z <- coVarsTrans.ScaleZ$Political.Stability - mean(coVarsTrans.ScaleZ$Political.Stability)/sd(coVarsTrans.ScaleZ$Political.Stability)

coVarsTrans.ScaleZ$Government.Effectiveness.z <- coVarsTrans.ScaleZ$Government.Effectiveness - mean(coVarsTrans.ScaleZ$Government.Effectiveness)/sd(coVarsTrans.ScaleZ$Government.Effectiveness)
coVarsTrans.ScaleZ$Rule.Law <- coVarsTrans.ScaleZ$Rule.Law - mean(coVarsTrans.ScaleZ$Rule.Law)/sd(coVarsTrans.ScaleZ$Rule.Law)
coVarsTrans.ScaleZ$Corruption.Control.z <- coVarsTrans.ScaleZ$Corruption.Control - mean(coVarsTrans.ScaleZ$Corruption.Control)/sd(coVarsTrans.ScaleZ$Corruption.Control)
coVarsTrans.ScaleZ$Reg.Quality.z <- coVarsTrans.ScaleZ$Reg.Quality - mean(coVarsTrans.ScaleZ$Reg.Quality)/sd(coVarsTrans.ScaleZ$Reg.Quality)

coVarsTrans.ScaleZ$PIKE.regional.z <- coVarsTrans.ScaleZ$PIKE.regional - mean(coVarsTrans.ScaleZ$PIKE.regional)/sd(coVarsTrans.ScaleZ$PIKE.regional)
coVarsTrans.ScaleZ$Definite.Probable.z <- coVarsTrans.ScaleZ$Definite.Probable - mean(coVarsTrans.ScaleZ$Definite.Probable)/sd(coVarsTrans.ScaleZ$Definite.Probable)
coVarsTrans.ScaleZ$Change.by.year.z <- coVarsTrans.ScaleZ$Change.by.year - mean(coVarsTrans.ScaleZ$Change.by.year)/sd(coVarsTrans.ScaleZ$Change.by.year)
coVarsTrans.ScaleZ$Diff.from.expected.z <- coVarsTrans.ScaleZ$Diff.from.expected - mean(coVarsTrans.ScaleZ$Diff.from.expected)/sd(coVarsTrans.ScaleZ$Diff.from.expected)


names(coVarsTrans.ScaleZ) 

coVarsTransScaleZ <- cbind(coVarsNoTrans,coVarsTrans.ScaleZ)
write.csv(coVarsTransScaleZ, "coVarsTrans.ScaleZ2.csv")


