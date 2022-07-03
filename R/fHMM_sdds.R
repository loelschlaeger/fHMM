#' Defining state-dependent distributions
#'
#' @description
#' This function defines state-dependent distributions for the {fHMM} package.
#'
#' @param sdds
#' A character or a character vector of length two that can be specified for
#' \code{"sdds"} in \code{\link{set_controls}}.
#'
#' @return
#' A list of length \code{length(sdds)}. Each element is a list, containing
#' the \code{"name"} of the distribution and a list \code{"pars"} of its 
#' parameters. Unknown parameters are set to \code{NULL}.
#'
#' @examples
#' sdds <- c("t(sigma = 0.1, df = Inf)", "gamma", "lnorm(mu = 1)")
#' fHMM:::fHMM_sdds(sdds)
#' 
#' @keywords 
#' internal

fHMM_sdds <- function(sdds) {
  out <- list()
  for (sdd in sdds) {
    sdd_tws <- gsub(" ", "", sdd)
    sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
    distr <- sdd_tws_split[1]
    if (!distr %in% c("t", "gamma", "lnorm")) {
      stop(paste(
        "Currently, only the t- ('t'), Gamma- ('gamma'), and log-normal",
        "('lnorm') distribution are implemented."), call. = FALSE)
    }
    if (is.na(sdd_tws_split[2])) {
      pars <- NULL
    } else {
      pars <- strsplit(strsplit(sdd_tws_split[2], split = c(","))[[1]], "=")
    }
    for (par in pars) {
      if (!par[1] %in% c("mu", "sigma", "df")) {
        pars[which(lapply(pars, function(x) x[1]) == par[1])] <- NULL
      }
    }
    names <- unlist(lapply(pars, `[[`, 1))
    pars <- lapply(
      pars,
      function(x) as.numeric(unlist(strsplit(x[2], split = "|", fixed = TRUE)))
    )
    names(pars) <- names
    if (distr %in% c("t","lnorm")) {
      pars[!names(pars) %in% c("mu", "sigma", "df")] <- NULL
    }
    if (distr == "gamma") {
      pars[!names(pars) %in% c("mu", "sigma", "df")] <- NULL
    }
    if (!is.null(pars$mu)) {
      if (distr %in% c("t","lnorm")) {
        if (!any(is_number(pars$mu))) {
          stop("'mu' must be a numeric.", call. = FALSE)
        }
      }
      if (distr == "gamma") {
        if (!any(is_number(pars$mu, pos = TRUE))) {
          stop("'mu' must be a positive numeric.", call. = FALSE)
        }
      }
    } else {
      pars$mu <- NULL
    }
    if (!is.null(pars$sigma)) {
      if (!any(is_number(pars$sigma, pos = TRUE))) {
        stop("'sigma' must be a positive numeric.", call. = FALSE)
      }
    } else {
      pars$sigma <- NULL
    }
    if (!is.null(pars$df)) {
      if (!any(is_number(pars$df, pos = TRUE))) {
        stop("'df' must be a positive numeric.", call. = FALSE)
      }
    } else if (distr == "t") {
      pars$df <- NULL
    }
    out[[length(out) + 1]] <- list("name" = distr, pars = pars)
  }
  class(out) <- "fHMM_sdds"
  return(out)
}

#' @noRd
#' @export

print.fHMM_sdds <- function(x, ...) {
  for (sdd in x) {
    cat(sdd$name)
    cat("(")
    cat(paste(names(sdd$pars), unlist(sapply(sdd$pars, paste, collapse = "|")),
      collapse = ", ", sep = " = "
    ))
    cat(")")
    cat(" ")
  }
  return(invisible(x))
}
