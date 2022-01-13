#' Compute (pseudo-) residuals.
#' @description
#' This function computes (pseudo-) residuals of the estimated model.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @return
#' An object of class \code{RprobitB_model}.
#' @export
#' @importFrom stats pt pgamma qnorm

compute_residuals <- function(x) {

  ### check if decoding is available
  if (is.null(x$decoding)) {
    warning("need decoding.")
    return(x)
  }

  ### function that computes the residuals
  cr <- function(data, mus, sigmas, dfs, sdd_name, decoding) {
    stopifnot(length(data) == length(decoding))
    out <- rep(NA, length(data))
    for (t in seq_along(data)) {
      if (sdd_name == "t") {
        Fxt <- stats::pt(
          q = (data[t] - mus[decoding[t]]) / sigmas[decoding[t]],
          df = dfs[decoding[t]]
        )
      }
      if (sdd_name == "gamma") {
        Fxt <- stats::pgamma(
          q = data[t],
          shape = mus[decoding[t]]^2 / sigmas[decoding[t]]^2,
          scale = sigmas[decoding[t]]^2 / mus[decoding[t]]
        )
      }
      out[t] <- stats::qnorm(Fxt)
    }
    return(out)
  }

  ### compute residuals
  par <- parUncon2par(x$estimate, x$data$controls)
  if (!x$data$controls$hierarchy) {
    residuals <- cr(
      data = x$data$data, mus = par$mus, sigmas = par$sigmas,
      dfs = par$dfs, sdd_name = x$data$controls$sdds[[1]]$name,
      decoding = x$decoding
    )
  } else {
    residuals <- matrix(NA, nrow = nrow(x$data$data), ncol = ncol(x$data$data))
    residuals[, 1] <- cr(
      data = x$data$data[, 1], mus = par$mus,
      sigmas = par$sigmas, dfs = par$dfs,
      sdd_name = x$data$controls$sdds[[1]]$name,
      decoding = x$decoding[, 1]
    )
    for (t in seq_len(nrow(residuals))) {
      curr <- x$decoding[t, 1]
      residuals[t, -1] <- cr(
        data = x$data$data[t, -1],
        mus = par$mus_star[[curr]],
        sigmas = par$sigmas_star[[curr]],
        dfs = par$dfs_star[[curr]],
        sdd_name = x$data$controls$sdds[[2]]$name,
        decoding = x$decoding[t, -1]
      )
    }
  }

  ### save residuals in 'x' and return 'x'
  message("Computed residuals")
  class(residuals) <- "fHMM_residuals"
  x$residuals <- residuals
  return(x)
}
