#######################################
### EXPLORE COVARIATES DATA
#######################################
coVars <-  read.xlsx("Data/master_file_2002.xlsx", sheetName = "Sheet1", rowIndex = 1:443)

names(coVars)


coVarsSub <- dplyr::select(coVars, c(6,9:49))
names(coVarsSub)

coVarsSub.scaled <- scale(coVarsSub[,2:42], center = TRUE, scale = TRUE)

clrs <- brewer.pal(10, "Spectral")

library(corrplot)
cors <- cor(coVarsSub.scaled, use = "pairwise")
quartz()
corrplot <- corrplot(cors, col = clrs, method="number", number.cex=0.4,
                     tl.cex=0.7, tl.col="black",
                     tl.srt=45)

coVarsSub05 <- filter(coVarsSub, year==2005)
coVarsSub05.scaled <- scale(coVarsSub02[,2:33], center = TRUE, scale = TRUE)
cors05 <- cor(coVarsSub05.scaled)
corrplot05 <- corrplot(cors05, col = clrs, method="number", number.cex=0.4,
                       tl.cex=0.7, tl.col="black",
                       tl.srt=45)
par(mfrow=c(2,1))
hist(coVars$NGDP_RPCH, col="gray", border="white", breaks=30, freq=FALSE)
hist(coVars$NGDPD, col="gray", border="white", breaks=30, freq=FALSE)
lambda1 <- coef(powerTransform(coVars$NGDPD))
hist(bcPower(coVars$NGDPD, lambda1), col="gray", border="white", breaks=30, freq=FALSE)
hist(log(coVars$NGDPD), col="gray", border="white", breaks=30, freq=FALSE)
summary(coVars$NGDPD)
hist(coVars$NGDPDPC, col="gray", border="white", breaks=30)
hist(log(coVars$NGDPDPC), col="gray", border="white", breaks=30)
hist(coVars$NGSD_NGDP, col="gray", border="white", breaks=30)
hist(coVars$PCPI, col="gray", border="white", breaks=30)
hist(log(coVars$PCPI), col="gray", border="white", breaks=30)
hist(coVars$PCPIPCH, col="gray", border="white", breaks=30)
hist(coVars$GGX_NGDP, col="gray", border="white", breaks=30)
hist(log(coVars$GGX_NGDP), col="gray", border="white", breaks=30)
hist(coVars$GGXCNL_NGDP, col="gray", border="white", breaks=30)
hist(coVars$GGXWDG_NGDP, col="gray", border="white", breaks=30)
hist(log(coVars$GGXWDG_NGDP), col="gray", border="white", breaks=30)
hist(coVars$BCA, col="gray", border="white", breaks=30)
hist(coVars$HDI, col="gray", border="white", breaks=30)
hist(log(coVars$HDI), col="gray", border="white", breaks=30)
hist(coVars$GNI, col="gray", border="white", breaks=30)
hist(log(coVars$GNI), col="gray", border="white", breaks=30)
hist(coVars$Resource.Depletion, col="gray", border="white", breaks=30)
hist(log(coVars$Resource.Depletion), col="gray", border="white", breaks=30)
hist(coVars$Adult.literacy, col="gray", border="white", breaks=30)
hist(coVars$Primary.ed.enrollment, col="gray", border="white", breaks=30)
hist(coVars$Mean.Schooling, col="gray", border="white", breaks=30)
hist(coVars$Total.pop, col="gray", border="white", breaks=30)
hist(log(coVars$Total.pop), col="gray", border="white", breaks=30)
hist(coVars$Pop.MultiDim.Povert, col="gray", border="white", breaks=30)
hist(log(coVars$Pop.MultiDim.Povert), col="gray", border="white", breaks=30)
hist(coVars$Deprivation.Intensity, col="gray", border="white", breaks=30)
hist(coVars$Pop.Below.National.Poverty, col="gray", border="white", breaks=30)
hist(coVars$PPP.125.day, col="gray", border="white", breaks=30)
hist(coVars$International.Dev.Aid, col="gray", border="white", breaks=30)
hist(log(coVars$International.Dev.Aid), col="gray", border="white", breaks=30)
hist(coVars$Corruption.Perception.Index, col="gray", border="white", breaks=30)
hist(log(coVars$Corruption.Perception.Index), col="gray", border="white", breaks=30)
hist(coVars$Voice.Accountability, col="gray", border="white", breaks=30)
hist(coVars$Political.Stability, col="gray", border="white", breaks=30)


ggplot(coVars, aes(x=GNI, y=reorder(country, GNI))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=HDI, y=reorder(country, HDI))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=GGXCNL_NGDP, y=reorder(country, GGXCNL_NGDP))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=PCPIPCH, y=reorder(country, PCPIPCH))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))



ggplot(coVars, aes(x=Total.pop, y=reorder(country, Total.pop))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=NGDPDPC, y=reorder(country, NGDPDPC))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=NGDPD, y=reorder(country, NGDPD))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Corruption.Perception.Index, y=reorder(country, Corruption.Perception.Index))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=Resource.Depletion, y=reorder(country, Resource.Depletion))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Mean.Schooling, y=reorder(country, Mean.Schooling))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Adult.literacy, y=reorder(country, Adult.literacy))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Pop.MultiDim.Povert, y=reorder(country, Pop.MultiDim.Povert))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=Pop.Severe.Multidim.Poverty, y=reorder(country, Pop.Severe.Multidim.Poverty))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=Voice.Accountability, y=reorder(country, Voice.Accountability))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(coVars, aes(x=Political.Stability, y=reorder(country, Political.Stability))) +
  geom_point(size=2) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Government.Effectiveness, y=reorder(country, Government.Effectiveness))) +
  geom_point(size=2) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Rule.Law, y=reorder(country, Rule.Law))) +
  geom_point(size=2) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Corruption.Control, y=reorder(country, Corruption.Control))) +
  geom_point(size=2) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVars, aes(x=Reg.Quality, y=reorder(country, Reg.Quality))) +
  geom_point(size=2) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))







coVarsIMFSummary <- coVars %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(NGDP_RPCH_mu = mean(NGDP_RPCH),
                   NGDP_RPCH_min = min(NGDP_RPCH),
                   NGDP_RPCH_max = max(NGDP_RPCH),
                   NGDP_RPCH_med = median(NGDP_RPCH),
                   NGDPD_mu = mean(NGDPD),
                   NGDPD_min = min(NGDPD),
                   NGDPD_max = max(NGDPD),
                   NGDPD_med = median(NGDPD),
                   NGDPDPC_mu = mean(NGDPDPC),
                   NGDPDPC_min = min(NGDPDPC),
                   NGDPDPC_max = max(NGDPDPC),
                   NGDPDPC_med = median(NGDPDPC),
                   NGSD_NGDP_mu = mean(NGSD_NGDP),
                   NGSD_NGDP_min = min(NGSD_NGDP),
                   NGSD_NGDP_max = max(NGSD_NGDP),
                   NGSD_NGDP_med = median(NGSD_NGDP),
                   PCPI_mu = mean(PCPI),
                   PCPI_min = min(PCPI),
                   PCPI_max = max(PCPI),
                   PCPI_med = median(PCPI),
                   PCPIPCH_mu = mean(PCPIPCH),
                   PCPIPCH_min = min(PCPIPCH),
                   PCPIPCH_max = max(PCPIPCH),
                   PCPIPCH_med = median(PCPIPCH),
                   GGX_NGDP_mu = mean(GGX_NGDP),
                   GGX_NGDP_min = min(GGX_NGDP),
                   GGX_NGDP_max = max(GGX_NGDP),
                   GGX_NGDP_med = median(GGX_NGDP),
                   GGXCNL_NGDP_mu = mean(GGXCNL_NGDP),
                   GGXCNL_NGDP_min = min(GGXCNL_NGDP),
                   GGXCNL_NGDP_max = max(GGXCNL_NGDP),
                   GGXCNL_NGDP_med = median(GGXCNL_NGDP),
                   GGXWDG_NGDP_mu = mean(GGXWDG_NGDP),
                   GGXWDG_NGDP_min = min(GGXWDG_NGDP),
                   GGXWDG_NGDP_max = max(GGXWDG_NGDP),
                   GGXWDG_NGDP_med = median(GGXWDG_NGDP),
                   BCA_mu = mean(BCA),
                   BCA_min = min(BCA),
                   BCA_max = max(BCA),
                   BCA_med = median(BCA))

write.csv(coVarsIMFSummary, "coVarsIMFSummary.csv")

coVarsPopSummary <- coVars %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(HDI_mu = mean(HDI),
                   HDI_med = median(HDI),
                   HDI_min = min(HDI),
                   HDI_max = max(HDI),
                   GNI_mu = mean(GNI),
                   GNI_med = median(GNI),
                   GNI_min = min(GNI),
                   GNI_max = max(GNI),
                   Resource.Depletion_mu = mean(Resource.Depletion),
                   Resource.Depletion_med = median(Resource.Depletion),
                   Resource.Depletion_min = min(Resource.Depletion),
                   Resource.Depletion_max = max(Resource.Depletion),
                   Adult.literacy_mu = mean(Adult.literacy),
                   Adult.literacy_med = median(Adult.literacy),
                   Adult.literacy_min = min(Adult.literacy),
                   Adult.literacy_max = max(Adult.literacy),
                   Primary.ed.enrollment_mu = mean(Primary.ed.enrollment),
                   Primary.ed.enrollment_med = median(Primary.ed.enrollment),
                   Primary.ed.enrollment_min = min(Primary.ed.enrollment),
                   Primary.ed.enrollment_max = max(Primary.ed.enrollment),
                   Mean.Schooling_mu = mean(Mean.Schooling),
                   Mean.Schooling_med = median(Mean.Schooling),
                   Mean.Schooling_min = min(Mean.Schooling),
                   Mean.Schooling_max = max(Mean.Schooling),
                   Total.pop_mu = mean(Total.pop),
                   Total.pop_med = median(Total.pop),
                   Total.pop_min = min(Total.pop),
                   Total.pop_max = max(Total.pop),
                   Pop.MultiDim.Povert_mu = mean(Pop.MultiDim.Povert),
                   Pop.MultiDim.Povert_med = median(Pop.MultiDim.Povert),
                   Pop.MultiDim.Povert_min = min(Pop.MultiDim.Povert),
                   Pop.MultiDim.Povert_max = max(Pop.MultiDim.Povert),
                   Deprivation.Intensity_mu = mean(Deprivation.Intensity),
                   Deprivation.Intensity_med = median(Deprivation.Intensity),
                   Deprivation.Intensity_min = min(Deprivation.Intensity),
                   Deprivation.Intensity_max = max(Deprivation.Intensity),
                   Pop.Below.National.Poverty_mu = mean(Pop.Below.National.Poverty),
                   Pop.Below.National.Poverty_med = median(Pop.Below.National.Poverty),
                   Pop.Below.National.Poverty_min = min(Pop.Below.National.Poverty),
                   Pop.Below.National.Poverty_max = max(Pop.Below.National.Poverty),
                   PPP.125.day_mu = mean(PPP.125.day),
                   PPP.125.day_med = median(PPP.125.day),
                   PPP.125.day_min = min(PPP.125.day),
                   PPP.125.day_max = max(PPP.125.day),
                   International.Dev.Aid_mu = mean(International.Dev.Aid),
                   International.Dev.Aid_med = median(International.Dev.Aid),
                   International.Dev.Aid_min = min(International.Dev.Aid),
                   International.Dev.Aid_max = max(International.Dev.Aid))
write.csv(coVarsPopSummary, "coVarsPopSummary.csv")

coVarsGovSummary <- coVars %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(Corruption.Perception.Index_mu = mean(Corruption.Perception.Index),
                   Corruption.Perception.Index_med = median(Corruption.Perception.Index),
                   Corruption.Perception.Index_min = min(Corruption.Perception.Index),
                   Corruption.Perception.Index_max = max(Corruption.Perception.Index),
                   Voice.Accountability_mu = mean(Voice.Accountability),
                   Voice.Accountability_med = median(Voice.Accountability),
                   Voice.Accountability_min = min(Voice.Accountability),
                   Voice.Accountability_max = max(Voice.Accountability),
                   Political.Stability_mu = mean(Political.Stability),
                   Political.Stability_med = median(Political.Stability),
                   Political.Stability_min = min(Political.Stability),
                   Political.Stability_max = max(Political.Stability),
                   Government.Effectiveness_mu = mean(Government.Effectiveness),
                   Government.Effectiveness_med = median(Government.Effectiveness),
                   Government.Effectiveness_min = min(Government.Effectiveness),
                   Government.Effectiveness_max = max(Government.Effectiveness),
                   Rule.Law_mu = mean(Rule.Law),
                   Rule.Law_med = median(Rule.Law),
                   Rule.Law_min = min(Rule.Law),
                   Rule.Law_max = max(Rule.Law),
                   Corruption.Control_mu = mean(Corruption.Control),
                   Corruption.Control_med = median(Corruption.Control),
                   Corruption.Control_min = min(Corruption.Control),
                   Corruption.Control_max = max(Corruption.Control),
                   Reg.Quality_mu = mean(Reg.Quality),
                   Reg.Quality_med = median(Reg.Quality),
                   Reg.Quality_min = min(Reg.Quality),
                   Reg.Quality_max = max(Reg.Quality))
write.csv(coVarsGovSummary, "coVarsGovSummary.csv")

# Read adjusted files back in
coVarsPopSumm <- read.xlsx("Data/coVarsPopSummary.xlsx", sheetName = "coVarsPopSummary")
names(coVarsPopSumm)
coVarsGovSumm <- read.xlsx("coVarsGovSummary.xlsx", sheetName = "coVarsGovSummary")
names(coVarsGovSumm)
coVarsIMFSumm <- read.xlsx("coVarsIMFSummary.xlsx", sheetName = "coVarsIMFSummary")
names(coVarsIMFSumm)

quartz()
ggplot(coVarsPopSumm, aes(x=as.numeric(HDI_mu), y=reorder(country, as.numeric(HDI_mu)))) +
  geom_point(size=2, shape=21) + # Use a larger dot
  geom_vline(xintercept = 17.74, colour="blue") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(coVarsPopSumm, aes(x=as.numeric(GNI_mu), y=reorder(country, as.numeric(GNI_mu)))) +
  geom_point(size=2, shape=21) + 
  geom_vline(xintercept = 18, colour="blue") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


### EXPLORE IMPUTED DATASETS
names(model1)
model1 <- read.csv("outdata081.csv")
model2 <- read.csv("outdata082.csv")
model3 <- read.csv("outdata083.csv")
model4 <- read.csv("outdata084.csv")
model5 <- read.csv("outdata085.csv")

quartz()
library(plyr)
m1GNI <- ddply(model1, "country", transform, trGNI = GNI-mean(GNI))
ggplot(m1GNI, aes(x=country, y=trGNI)) +
  geom_tufteboxplot() +
  ggtitle("GNI by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(model2, "country", transform, trGNI = GNI-mean(GNI))
ggplot(m2GNI, aes(x=country, y=trGNI)) +
  geom_tufteboxplot() +
  ggtitle("GNI by Country (Model2)") +
theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(model3, "country", transform, trGNI = GNI-mean(GNI))
ggplot(m3GNI, aes(x=country, y=trGNI)) +
  geom_tufteboxplot() +
  ggtitle("GNI by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(model4, "country", transform, trGNI = GNI-mean(GNI))
ggplot(m4GNI, aes(x=country, y=trGNI)) +
  geom_tufteboxplot() +
  ggtitle("GNI by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(model5, "country", transform, trGNI = GNI-mean(GNI))
ggplot(m5GNI, aes(x=country, y=trGNI)) +
  geom_tufteboxplot() +
  ggtitle("GNI by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())


m1GNI <- ddply(model1, "country", transform, trHDI = HDI-mean(HDI))
ggplot(m1GNI, aes(x=country, y=trHDI)) +
  geom_tufteboxplot() +
  ggtitle("HDI by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(model2, "country", transform, trHDI = HDI-mean(HDI))
ggplot(m2GNI, aes(x=country, y=trHDI)) +
  geom_tufteboxplot() +
  ggtitle("HDI by Country (Model2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(model3, "country", transform, trHDI = HDI-mean(HDI))
ggplot(m3GNI, aes(x=country, y=trHDI)) +
  geom_tufteboxplot() +
  ggtitle("HDI by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(model4, "country", transform, trHDI = HDI-mean(HDI))
ggplot(m4GNI, aes(x=country, y=trHDI)) +
  geom_tufteboxplot() +
  ggtitle("HDI by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(model5, "country", transform, trHDI = HDI-mean(HDI))
ggplot(m5GNI, aes(x=country, y=trHDI)) +
  geom_tufteboxplot() +
  ggtitle("HDI by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())

names(m1GNI)

m1GNI <- ddply(m1GNI, "country", transform, trMean.Schooling = Mean.Schooling-median(Mean.Schooling))
ggplot(m1GNI, aes(x=country, y=trMean.Schooling)) +
  geom_tufteboxplot() +
  ggtitle("Mean.Schooling by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(m2GNI, "country", transform, trMean.Schooling = Mean.Schooling-median(Mean.Schooling))
ggplot(m2GNI, aes(x=country, y=trMean.Schooling)) +
  geom_tufteboxplot() +
  ggtitle("Mean.Schooling by Country (Model2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(m3GNI, "country", transform, trMean.Schooling = Mean.Schooling-median(Mean.Schooling))
ggplot(m3GNI, aes(x=country, y=trMean.Schooling)) +
  geom_tufteboxplot() +
  ggtitle("Mean.Schooling by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(m4GNI, "country", transform, trMean.Schooling = Mean.Schooling-median(Mean.Schooling))
ggplot(m4GNI, aes(x=country, y=trMean.Schooling)) +
  geom_tufteboxplot() +
  ggtitle("Mean.Schooling by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(m5GNI, "country", transform, trMean.Schooling = Mean.Schooling-median(Mean.Schooling))
ggplot(m5GNI, aes(x=country, y=trMean.Schooling)) +
  geom_tufteboxplot() +
  ggtitle("Mean.Schooling by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())


m1GNI <- ddply(m1GNI, "country", transform, trResource.Depletion = Resource.Depletion-mean(Resource.Depletion))
ggplot(m1GNI, aes(x=country, y=trResource.Depletion)) +
  geom_tufteboxplot() +
  ggtitle("Resource.Depletion by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(m2GNI, "country", transform, trResource.Depletion = Resource.Depletion-mean(Resource.Depletion))
ggplot(m2GNI, aes(x=country, y=trResource.Depletion)) +
  geom_tufteboxplot() +
  ggtitle("Resource.Depletion by Country (Model2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(m3GNI, "country", transform, trResource.Depletion = Resource.Depletion-mean(Resource.Depletion))
ggplot(m3GNI, aes(x=country, y=trResource.Depletion)) +
  geom_tufteboxplot() +
  ggtitle("Resource.Depletion by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(m4GNI, "country", transform, trResource.Depletion = Resource.Depletion-mean(Resource.Depletion))
ggplot(m4GNI, aes(x=country, y=trResource.Depletion)) +
  geom_tufteboxplot() +
  ggtitle("Resource.Depletion by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(m5GNI, "country", transform, trResource.Depletion = Resource.Depletion-mean(Resource.Depletion))
ggplot(m5GNI, aes(x=country, y=trResource.Depletion)) +
  geom_tufteboxplot() +
  ggtitle("Resource.Depletion by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())


m1GNI <- ddply(m1GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m1GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(m2GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m2GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(m3GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m3GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(m4GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m4GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(m5GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m5GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())


m1GNI <- ddply(m1GNI, "country", transform, trDiff.from.expected = Diff.from.expected-mean(Diff.from.expected))
ggplot(m1GNI, aes(x=country, y=Diff.from.expected)) +
  geom_tufteboxplot() +
  ggtitle("Diff.from.expected by Country (Model1)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m2GNI <- ddply(m2GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m2GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m3GNI <- ddply(m3GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m3GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m4GNI <- ddply(m4GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m4GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())
m5GNI <- ddply(m5GNI, "country", transform, trCorruption.Perception.Index = Corruption.Perception.Index-mean(Corruption.Perception.Index))
ggplot(m5GNI, aes(x=country, y=trCorruption.Perception.Index)) +
  geom_tufteboxplot() +
  ggtitle("Corruption.Perception.Index by Country (Model5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())

m4GNI <- ddply(m4GNI, "country", transform, trPop.Below.National.Poverty = Pop.Below.National.Poverty-mean(Pop.Below.National.Poverty))
ggplot(m4GNI, aes(x=country, y=trPop.Below.National.Poverty)) +
  geom_tufteboxplot() +
  ggtitle("Pop.Below.National.Poverty by Country (Model4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = rel(0.8)),
        axis.title.x=element_blank())

