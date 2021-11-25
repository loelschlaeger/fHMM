#' Visualization of log-likelihood values.
#' @description
#' This function plots the log-likelihood values of the different estimation runs.
#' @param lls
#' A numeric vector of log-likelihood values.
#' @return
#' No return value.

plot_ll <- function(lls) {
  if (length(lls) <= 5) {
    plot(lls,
      xaxt = "n", yaxt = "n", xlab = "Estimation run", ylab = "",
      main = "Log-likelihoods", pch = 16,
      ylim = c(floor(min(lls, na.rm = TRUE)), ceiling(max(lls, na.rm = TRUE)))
    )
    axis(1, las = 1, at = seq_len(length(lls)), labels = seq_len(length(lls)))
  } else {
    plot(lls,
      yaxt = "n", xlab = "Estimation run", ylab = "",
      main = "Log-likelihoods", pch = 16,
      ylim = c(floor(min(lls, na.rm = TRUE)), ceiling(max(lls, na.rm = TRUE)))
    )
  }
  points(
    x = which.max(lls), y = lls[which.max(lls)], pch = 16, cex = 1.25,
    col = "red"
  )
  axis(2,
    las = 1, at = unique(round(lls[!is.na(lls)])),
    labels = unique(round(lls[!is.na(lls)]))
  )
}
