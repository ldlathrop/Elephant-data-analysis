library(MCMCglmm)
library(blme)
library(nlme)
theme_set(theme_bw())
library(grid)          ## for unit()
zmargin <- theme(panel.margin=unit(0,"lines")) ## to squash facets together ...
library(scales)        ## for squish()
library(gridExtra)     ## for grid.arrange()
library(proto)         ## for horizontal line range plot
source("geom-linerangeh.R")  ## for horizontal line ranges
library(coefplot2) ## coefficient plots
library(coda)      ## MCMC diagnostics
library(aods3)     ## overdispersion diagnostics
library(scapeMCMC) ## pretty plots from MCMC fits
library(bbmle)     ## AICtab
library(pbkrtest)  ## parametric bootstrap
library(Hmisc)
## library(plotrix)  ## for plotCI
## library(emdbook)  ## for curve3d()

install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R",
                      type="source")
require(devtools)
install_url("https://cran.r-project.org/src/contrib/Archive/scapeMCMC/scapeMCMC_1.1-3.tar.gz")
names(coVars)
model1 <- read.csv("outdata081.csv")
model1$year <- as.factor(model1$year)
mod1_lme <- lme(Diff.from.expected ~ year,
                data=model1, method="REML",
                random = ~ 1 + year | country, 
                correlation=corAR1(form=~year|country),
                weights=varFixed(~I(1/n)),
                control=list(maxIter=10000, niterEM=10000))
genMixModel1 <- bglmer(Diff.from.expected ~  (1 | country) + (1 | year),
                      data = model1, family = gaussian(link = log),
                      cov.prior = wishart, fixef.prior = normal)