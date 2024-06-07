pkgname <- "EBMAforecast"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('EBMAforecast')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("EBMAforecast-package")
### * EBMAforecast-package

flush(stderr()); flush(stdout())

### Name: EBMAforecast-package
### Title: Ensemble BMA For Binary Dependent Variables
### Aliases: EBMAforecast-package EBMAforecast
### Keywords: package

### ** Examples

demo(EBMAforecast)





cleanEx()
nameEx("Ensemble.logit")
### * Ensemble.logit

flush(stderr()); flush(stdout())

### Name: Ensemble.logit
### Title: Ensemble BMA Logit Function for Binary Outcomes
### Aliases: Ensemble.logit
### Keywords: package BMA

### ** Examples

demo(EBMAforecast)



cleanEx()
nameEx("Insample")
### * Insample

flush(stderr()); flush(stdout())

### Name: Insample
### Title: In-Sample Predictions from Seven Forecasting Models
### Aliases: Insample
### Keywords: datasets

### ** Examples

data(Insample)
summary(Insample)



cleanEx()
nameEx("Outsample")
### * Outsample

flush(stderr()); flush(stdout())

### Name: Outsample
### Title: Out-of-Sample Predictions from the Seven Forecasting Models
### Aliases: Outsample
### Keywords: datasets

### ** Examples

data(Outsample)



cleanEx()
nameEx("predict.Ensemble.logit")
### * predict.Ensemble.logit

flush(stderr()); flush(stdout())

### Name: predict.Ensemble.logit
### Title: Generate Out-of-Sample Predictions Using Ensemble BMA Models for
###   Binary
### Aliases: predict.Ensemble.logit
### Keywords: BMA

### ** Examples

demo(EBMAforecast)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
