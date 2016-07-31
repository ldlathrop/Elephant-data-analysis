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
