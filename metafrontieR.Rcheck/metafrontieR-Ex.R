pkgname <- "metafrontieR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('metafrontieR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("sfametafrontier")
### * sfametafrontier

flush(stderr()); flush(stdout())

### Name: sfametafrontier
### Title: Stochastic metafrontier estimation
### Aliases: sfametafrontier print.sfametafrontier
### Keywords: metafrontier models optimize

### ** Examples

## Not run: 
##D ## Standard SFA metafrontier (LP) with ricephil data
##D data("ricephil")
##D ricephil$group <- cut(ricephil$AREA,
##D   breaks = c(0, 1, 2, Inf),
##D   labels = c("small", "medium", "large")
##D )
##D 
##D meta_lp <- sfametafrontier(
##D   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK) + log(OTHER),
##D   data = ricephil, group = "group", S = 1, udist = "hnormal",
##D   metaMethod = "lp"
##D )
##D summary(meta_lp)
##D 
##D ## Huang (2014) two-stage SFA metafrontier
##D meta_huang <- sfametafrontier(
##D   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK) + log(OTHER),
##D   data = ricephil, group = "group", S = 1, udist = "hnormal",
##D   metaMethod = "sfa", sfaApproach = "huang"
##D )
##D summary(meta_huang)
##D 
##D ## Latent class group models
##D meta_lcm <- sfametafrontier(
##D   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK) + log(OTHER),
##D   data = ricephil, group = "group", S = 1,
##D   groupType = "sfalcmcross", lcmClasses = 2, metaMethod = "lp"
##D )
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
