#' Define state-dependent distributions
#'
#' @description
#' This helper function defines state-dependent distributions.
#'
#' @inheritParams set_controls
#'
#' @return
#' A \code{list} of length \code{1} (or \code{2} in the hierarchical case). 
#' Each element again is a \code{list}, containing
#' * the \code{"name"} of the distribution 
#' * and a list \code{"pars"} of its parameters, where unknown parameters are 
#'   set to \code{NULL}.
#' 
#' @keywords internal

fHMM_sdds <- function(sdds, states) {
  
  ### input checks
  if (inherits(sdds, "fHMM_sdds")) {
    return(sdds)
  }
  if (!checkmate::test_atomic_vector(states)) {
    stop(
      "The control 'states' must be a vector.", 
      call. = FALSE
    )
  }
  if (length(states) == 1) {
    hierarchy <- FALSE
    if (!checkmate::test_integerish(states, lower = 2, len = 1)) {
      stop(
        "The control 'states' must be an integer greater or equal 2.",
        call. = FALSE
      )
    }
  } else if (length(states) == 2) {
    hierarchy <- TRUE
    if (!checkmate::test_integerish(states, lower = 2, len = 2)) {
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
  if (!checkmate::test_character(
    sdds, any.missing = FALSE, len = ifelse(hierarchy, 2, 1))
  ) {
    stop(
      "The control 'sdds' must be a character ", 
      if (hierarchy) "vector ", "of length ", ifelse(hierarchy, 2, 1), ".",
      call. = FALSE
    )
  }
  
  ### decode state-dependent distribution specification
  out <- list()
  for (i in if (hierarchy) 1:2 else 1) {
    sdd <- sdds[i]
    checkmate::assert_string(sdd)
    sdd_tws <- gsub(" ", "", sdd)
    sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
    distr <- sdd_tws_split[1]
    if (!distr %in% c("normal", "lognormal", "t", "gamma", "poisson")) {
      stop(
        paste0(
          "Currently, only the following distributions are implemented:\n",
          "- normal distribution ('normal')\n", 
          "- log-normal distribution ('lognormal')\n", 
          "- t-distribution ('t')\n",
          "- Gamma distribution ('gamma')\n", 
          "- Poisson distribution ('poisson')"
        ), 
        call. = FALSE
      )
    }
    if (is.na(sdd_tws_split[2])) {
      pars <- list()
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
    for (par in names(pars)) {
      if (!length(pars[[par]]) %in% c(1, states[i])) {
        stop(
          "Fixed values for the parameter '", par, 
          "' must be of length 1 or ", states[i], ".",
          call. = FALSE
        )
      }
    }
    if (distr %in% c("t")) {
      pars[!names(pars) %in% c("mu", "sigma", "df")] <- NULL
    }
    if (distr %in% c("normal", "lognormal", "gamma")) {
      pars[!names(pars) %in% c("mu", "sigma")] <- NULL
    }
    if (distr %in% c("poisson")) {
      pars[!names(pars) %in% c("mu")] <- NULL
    }
    if (!is.null(pars$mu)) {
      if (distr %in% c("gamma", "poisson")) {
        if (!checkmate::test_numeric(pars$mu) || any(pars$mu <= 0)) {
          stop("'mu' must be a positive numeric.", call. = FALSE)
        }
      }
    } else {
      pars$mu <- NULL
    }
    if (!is.null(pars$sigma)) {
      if (!checkmate::test_numeric(pars$sigma) || any(pars$sigma <= 0)) {
        stop("'sigma' must be a positive numeric.", call. = FALSE)
      }
    } else {
      pars$sigma <- NULL
    }
    if (!is.null(pars$df)) {
      if (!checkmate::test_numeric(pars$df) || any(pars$df <= 0)) {
        stop("'df' must be a positive numeric.", call. = FALSE)
      }
    } else if (distr == "t") {
      pars$df <- NULL
    }
    out[[i]] <- list("name" = distr, "pars" = pars)
  }
  structure(out, class = c("fHMM_sdds", "list"))
}

#' @rdname fHMM_sdds
#' @param ...
#' Currently not used.
#' @exportS3Method 

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
