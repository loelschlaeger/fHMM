print.fHMM_model = function() {
  writeLines(paste0("Estimation results of model '",controls[["id"]],"':\n"))
  writeLines(sprintf("%-15s %.2f","log-likelihood:",fit[["logLikelihood"]]))
  writeLines(sprintf("%-15s %.2f","AIC:",fit[["AIC"]]))
  writeLines(sprintf("%-15s %.2f","BIC:",fit[["BIC"]]))
  writeLines(sprintf("%-15s %.0f","exit code:",mod[["code"]]))
  writeLines(sprintf("%-15s %.0f","iterations:",mod[["iterations"]])); cat("\n")
}