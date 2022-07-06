try(setwd("jss"),silent=TRUE)
Sweave("fhmm_oelschlaeger_adam_michels.Rnw")
tinytex::pdflatex("fhmm_oelschlaeger_adam_michels.tex")
# Stangle("fhmm_oelschlaeger_adam_michels.Rnw")