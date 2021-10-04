### R code from vignette source 'fhmm_oelschlaeger_adam_michels.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("fHMM")


###################################################
### code chunk number 2: some estimated model
###################################################
controls = list("fit" = list("runs" = 5))
fHMM::fit_hmm(controls)


