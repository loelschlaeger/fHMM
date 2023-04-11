#' Define state-dependent distributions
#'
#' @description
#' This helper function defines state-dependent distributions for the \{fHMM\} 
#' package.
#'
#' @inheritParams set_controls
#'
#' @return
#' A \code{list} of length \code{length(sdds)}. 
#' Each element again is a \code{list}, containing
#' * the \code{"name"} of the distribution 
#' * and a list \code{"pars"} of its parameters, where unknown parameters are 
#'   set to \code{NULL}.
#'
#' @examples
#' \dontrun{
#' fHMM_sdds(
#'   sdds = c("t(sigma = 0.1 | 1, df = Inf)", "gamma"),
#'   states = c(2, 3)
#' )
#' }
#' 
#' @keywords internal

fHMM_sdds <- function(sdds, states) {
  
  ### input checks
  if (length(states) == 1) {
    hierarchy <- FALSE
    if (!(all(is_number(states, int = TRUE)) && all(states >= 2))) {
      stop(
        "The control 'states' must be an integer greater or equal 2.",
        call. = FALSE
      )
    }
  } else if (length(states) == 2) {
    hierarchy <- TRUE
    if (!(all(is_number(states, int = TRUE)) && all(states >= 2))) {
      stop(
        "The control 'states' must be a vector of integers greater or equal 2.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "The control 'states' must be a vector of length 1 or 2.",
      call. = FALSE
    )
  }
  if (inherits(sdds, "fHMM_sdds")) {
    sdds <- sapply(sdds, function(x) x$label)
  }
  if (!is.character(sdds) || length(sdds) != ifelse(hierarchy, 2, 1)) {
    stop(
      "The control 'sdds' must be a character ", 
      if (hierarchy) "vector ", "of length ", ifelse(hierarchy, 2, 1), ".",
      call. = FALSE
    )
  }
  
  ### create 'fHMM_sdds' object
  out <- list()
  for (i in if (hierarchy) 1:2 else 1) {
    sdd <- sdds[i]
    sdd_tws <- gsub(" ", "", sdd)
    sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
    distr <- sdd_tws_split[1]
    if (!distr %in% c("t", "gamma", "lnorm", "poisson")) {
      stop(
        paste0(
          "Currently, only the following distributions are implemented:\n",
          "t- ('t'), Gamma- ('gamma'), log-normal, ('lnorm'), and poisson- ",
          "('poisson') distribution"
        ), 
        call. = FALSE
      )
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
    if (distr == "t") {
      pars[!names(pars) %in% c("mu", "sigma", "df")] <- NULL
    }
    if (distr %in% c("gamma", "lnorm")) {
      pars[!names(pars) %in% c("mu", "sigma")] <- NULL
    }
    if (distr == "poisson") {
      pars[!names(pars) %in% c("mu")] <- NULL
    }
    if (!is.null(pars$mu)) {
      if (distr == "gamma") {
        if (!any(is_number(pars$mu, pos = TRUE))) {
          stop("'mu' must be a positive numeric.", call. = FALSE)
        }
      }
    } 
    if (!is.null(pars$sigma)) {
      if (!any(is_number(pars$sigma, pos = TRUE))) {
        stop("'sigma' must be a positive numeric.", call. = FALSE)
      }
    } 
    if (!is.null(pars$df)) {
      if (!any(is_number(pars$df, pos = TRUE))) {
        stop("'df' must be a positive numeric.", call. = FALSE)
      }
    }
    label <- paste0(
      distr, "(",
      paste(
        names(pars), unlist(sapply(pars, paste, collapse = "|")),
        collapse = ", ", sep = " = "
      ),
      ")"
    )
    for (p in seq_along(pars)) {
      if (length(pars[[p]]) == 1) {
        pars[[p]] <- rep(pars[[p]], states[i])
      }
      if (length(pars[[p]]) != states[i]) {
        stop(
          "Number of fixed parameters in '", label, 
          "' does not fit the number of states (", states[i], ").",
          call. = FALSE
        )
      }
    }
    out[[length(out) + 1]] <- list(
      "name" = distr, "pars" = pars, "label" = label
    )
  }
  class(out) <- c("fHMM_sdds", "list")
  return(out)
}

#' @rdname fHMM_sdds
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_sdds <- function(x, ...) {
  if (!inherits(x, "fHMM_sdds")) {
    stop("Not of class 'fHMM_sdds'.", call. = FALSE)
  }
  if (length(x) == 1) {
    cat(x[[1]]$label)
  } else {
    cat("coarse-scale ", x[[1]]$label, ", fine-scale ", x[[2]]$label, 
        sep = "")
  }
  return(invisible(x))
}
