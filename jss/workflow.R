try(setwd("jss"),silent=TRUE)
Sweave("fhmm_oelschlaeger_adam_michels.Rnw")
tools::texi2pdf("fhmm_oelschlaeger_adam_michels.tex")
Stangle("fhmm_oelschlaeger_adam_michels.Rnw")

