library(Zelig)
library(mitools)
library(lme4)
library(MCMCglmm)
library(blme)
library(nlme)
theme_set(theme_bw())
library(grid)          ## for unit()
zmargin <- theme(panel.margin=unit(0,"lines")) ## to squash facets together ...
library(scales)        ## for squish()
library(gridExtra)     ## for grid.arrange()
library(proto)         ## for horizontal line range plot
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

summary(a.coVarsTrans.more)

str(a.coVarsTrans.more$imputations)

## Zelig models
## zelig(Y ~ X1 + X2, order=c(1,0,0), model = "arima", data = mydata)
z1 <- zarima$new()
z1$zelig(Diff.from.expected~HDI, order=c(1,0,1), model="arima",
               data = a.coVarsTrans.more, ts="year", cs="country")

