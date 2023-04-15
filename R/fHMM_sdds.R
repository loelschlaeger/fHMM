#' Define state-dependent distributions
#'
#' @description
#' This helper function defines state-dependent distributions for the \{fHMM\} 
#' package.
#'
#' @inheritParams set_controls
#'
#' @return
#' An object of class \code{"fHMM_sdds"}, which is a \code{list} of length 
#' \code{length(sdds)}. Each element again is a \code{list}, containing
#' \itemize{
#'   \item \code{"distr_class"}, a \code{character} which defines the 
#'   distribution class and can be one of
#'   \itemize{
#'     \item \code{"norm"} (normal distribution),
#'     \item \code{"t"} (t-distribution),
#'     \item \code{"gamma"} (gamma distribution),
#'     \item \code{"lnorm"} (log-normal distribution),
#'     \item \code{"poisson"} (poisson distribution),
#'   }
#'   \item \code{"label"}, a \code{character} label for the specified 
#'         distribution,
#'   \item \code{"fixed_pars"}, a \code{list} of fixed parameter values,
#'   \item \code{"sample"}, a \code{function} to sample from the state-dependent
#'         distribution,
#'   \item \code{"density"}, a \code{function} to compute the density of the 
#'         state-dependent distribution,
#'   \item \code{"distribution"}, a \code{function} to compute the cumulative
#'         density of the state-dependent distribution.
#' }
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
  if (!is.atomic(states)) {
    stop(
      "The control 'states' must be a vector.", 
      call. = FALSE
    )
  }
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
  out <- list()
  for (i in if (hierarchy) 1:2 else 1) {
    out[[i]] <- decode_sdd(sdd = sdds[i], states = states[i])
  }
  class(out) <- c("fHMM_sdds", "list")
  return(out)
}

#' Decode specification for state-dependent distribution
#' 
#' @description 
#' This helper function decodes the specification for a state-dependent
#' distribution.
#' 
#' @param sdd
#' A single specification for the \code{"sdds"} control,
#' see \code{\link{set_controls}}.
#' @param states
#' A single specification for the \code{"states"} control,
#' see \code{\link{set_controls}}.
#' 
#' @return 
#' A \code{list} with the elements
#' \itemize{
#'   \item \code{"distr_class"}, a \code{character} which defines the 
#'   distribution class and can be one of
#'   \itemize{
#'     \item \code{"norm"} (normal distribution),
#'     \item \code{"t"} (t-distribution),
#'     \item \code{"gamma"} (gamma distribution),
#'     \item \code{"lnorm"} (log-normal distribution),
#'     \item \code{"poisson"} (poisson distribution),
#'   }
#'   \item \code{"label"}, a \code{character} label for the specified 
#'         distribution,
#'   \item \code{"fixed_pars"}, a \code{list} of fixed parameter values,
#'   \item \code{"sample"}, a \code{function} to sample from the state-dependent
#'         distribution,
#'   \item \code{"density"}, a \code{function} to compute the density of the 
#'         state-dependent distribution,
#'   \item \code{"distribution"}, a \code{function} to compute the cumulative
#'         density of the state-dependent distribution.
#' }
#' 
#' @examples 
#' \dontrun{
#' decode_sdd("t(mu = 1)", states = 3)
#' }
#' 
#' @keywords internal

decode_sdd <- function(sdd, states) {
  stopifnot(is.character(sdd), length(sdd) == 1)
  stopifnot(
    is.numeric(states), length(states) == 1, 
    is_number(states, int = TRUE, pos = TRUE), states >= 2
  )
  sdd_tws <- gsub(" ", "", sdd)
  sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
  distr_class <- sdd_tws_split[1]
  if (!distr_class %in% c("norm", "t", "gamma", "lnorm", "poisson")) {
    stop(
      paste0(
        "Currently, only the following distributions are implemented:\n",
        "normal- ('norm'), t- ('t'), gamma- ('gamma'), log-normal- ('lnorm')",
        "and poisson- ('poisson') distribution."
      ), 
      call. = FALSE
    )
  }
  if (is.na(sdd_tws_split[2])) {
    fixed_pars <- list()
  } else {
    fixed_pars <- strsplit(strsplit(sdd_tws_split[2], split = c(","))[[1]], "=")
  }
  par_names <- unlist(lapply(fixed_pars, `[[`, 1))
  names(fixed_pars) <- par_names
  check_par_names_allowed <- function(fixed_pars, par_names_allowed) {
    par_names_bad <- setdiff(names(fixed_pars), par_names_allowed)
    if (length(par_names_bad) > 0) {
      warning(
        "The parameters '", paste(par_names_bad, collapse = ", "), 
        "' in '", sdd, "' are ignored.\n",
        "Only the following parameters can be specified:",
        if ("mu" %in% par_names_allowed) "\n- mu: the expected value",
        if ("sigma" %in% par_names_allowed) "\n- sigma: the standard deviation",
        if ("df" %in% par_names_allowed) "\n- df: the degrees of freedom",
        call. = FALSE
      )
    }
    fixed_pars[par_names_bad] <- NULL
    return(fixed_pars)
  }
  fixed_pars <- check_par_names_allowed(fixed_pars, c("mu", "sigma", "df"))
  fixed_pars <- lapply(
    fixed_pars, function(x) {
      as.numeric(unlist(strsplit(x[2], split = "|", fixed = TRUE)))
    }
  )
  if (distr_class == "t") {
    fixed_pars <- check_par_names_allowed(fixed_pars, c("mu", "sigma", "df"))
  }
  if (distr_class %in% c("gamma", "lnorm")) {
    fixed_pars <- check_par_names_allowed(fixed_pars, c("mu", "sigma"))
  }
  if (distr_class == "poisson") {
    fixed_pars <- check_par_names_allowed(fixed_pars, c("mu"))
  }
  if ("mu" %in% names(fixed_pars)) {
    if (distr_class == "gamma") {
      if (!any(is_number(fixed_pars$mu, pos = TRUE))) {
        stop(
          "'mu' must be positive for the gamma distribution.", 
          call. = FALSE
        )
      }
    }
  } 
  if ("sigma" %in% names(fixed_pars)) {
    if (!any(is_number(fixed_pars$sigma, pos = TRUE))) {
      stop("'sigma' must be positive.", call. = FALSE)
    }
  } 
  if ("df" %in% names(fixed_pars)) {
    if (!any(is_number(fixed_pars$df, pos = TRUE))) {
      stop("'df' must be positive.", call. = FALSE)
    }
  }
  label <- paste0(
    distr_class, "(",
    paste(
      names(fixed_pars), unlist(sapply(fixed_pars, paste, collapse = "|")),
      collapse = ", ", sep = " = "
    ),
    ")"
  )
  for (p in seq_along(fixed_pars)) {
    if (length(fixed_pars[[p]]) == 1) {
      fixed_pars[[p]] <- rep(fixed_pars[[p]], states)
    }
    if (length(fixed_pars[[p]]) != states) {
      stop(
        "Number of fixed parameters in '", label, 
        "' does not fit the number of states (", states, ").",
        call. = FALSE
      )
    }
  }
  list(
    "distr_class" = distr_class,
    "name" = distr_class, # TODO: REMOVE LATER
    "label" = label,
    "fixed_pars" = fixed_pars,
    "pars" = fixed_pars,  # TODO: REMOVE LATER
    "sample" = build_sample_function(distr_class),
    "density" = NA, # TODO: implement build_density_function(distr_class),
    "distribution" = NA # TODO: implement build_distribution_function(distr_class)
  )
}

#' Build sampling function for state-dependent distribution
#'
#' @description 
#' This helper function builds a \code{function} that can sample from a
#' state-dependent distribution.
#' 
#' @param distr_class
#' A \code{character} which defines the class of the state-dependent 
#' distribution class and can be one of
#' \itemize{
#'   \item \code{"norm"} (normal distribution),
#'   \item \code{"t"} (t-distribution),
#'   \item \code{"gamma"} (gamma distribution),
#'   \item \code{"lnorm"} (log-normal distribution),
#'   \item \code{"poisson"} (poisson distribution),
#' }
#' 
#' @return 
#' A \code{function} which returns random samples from the state-dependent
#' distribution and has the arguments
#' \itemize{
#'   \item \code{n}, the number of samples (by default \code{n = 1}),
#'   \item \code{mu}, \code{sigma}, and \code{df}, vectors for the mean, 
#'         standard deviation, and degrees of freedom, respectively 
#'         (if required), in each state,
#'   \item \code{state}, the state index.
#' }
#' 
#' @examples
#' \dontrun{
#' sample_poisson_sdd <- build_sample_function("poisson")
#' mu <- c(1, 10)
#' sample_poisson_sdd(mu = mu, state = 1)
#' sample_poisson_sdd(mu = mu, state = 2)
#' }
#' 
#' @keywords internal  

build_sample_function <- function(distr_class) {
  stopifnot(is.character(distr_class), length(distr_class) == 1)
  stopifnot(distr_class %in% c("norm", "t", "gamma", "lnorm", "poisson"))
  if (distr_class == "norm") {
    function (n = 1, mu, sigma, state) {
      stats::rnorm(n, mean = mu[state], sd = sigma[state])
    }
  } else if (distr_class == "t") {
    function (n = 1, mu, sigma, df, state) {
      stats::rt(n = n, df[state]) * sigma[state] + mu[state]
    }
  } else if (distr_class == "gamma") {
    function (n = 1, mu, sigma, state) {
      stats::rgamma(
        n = n, shape = mu[state]^2 / sigma[state]^2,
        scale = sigma[state]^2 / mu[state]
      )
    } 
  } else if (distr_class == "lnorm") {
    function (n = 1, mu, sigma, state) {
      stats::rlnorm(n = n, meanlog = mu[state], sdlog = sigma[state])
    }
  } else if (distr_class == "poisson") {
    function (n = 1, mu, state) {
      stats::rpois(n = n, lambda = mu[state])
    }
  }
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
    cat("coarse-scale ", x[[1]]$label, ", fine-scale ", x[[2]]$label, sep = "")
  }
  return(invisible(x))
}

