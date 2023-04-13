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
    out <- encode_sdd(sdds[i])
    class <- out$class
    fixed_parameters <- out$fixed_parameters
    label <- out$label
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
    cat("coarse-scale ", x[[1]]$label, ", fine-scale ", x[[2]]$label, sep = "")
  }
  return(invisible(x))
}

#' Clean fixed parameters for state-dependent distribution
#' 
#' @description 
#' This helper function checks the fixed parameters for a state-dependent
#' distribution.
#' 
#' @param class
#' One of \code{"norm"}, \code{"t"}, \code{"gamma"}, \code{"lnorm"},
#' \code{"poisson"}.
#' @param nstates
#' An \code{integer}, the number of states.
#' @param ...
#' TODO
#'
#' @return 
#' A \code{list} ... TODO

clean_fixed_parameters <- function(class, nstates, ...) {
  
}

#' Build \code{function} to draw from state-dependent distribution
#'
#' @description 
#' This helper function builds a \code{function} that can draw from a
#' state-dependent distribution.
#' 
#' @inheritParams clean_fixed_parameters
#' @param fixed_parameters
#' A named \code{list} of fixed parameters for the state-dependent distribution.
#' In most cases, it is the output of \code{\link{clean_fixed_parameters}}.
#' 
#' @return 
#' A \code{function} that returns random draws. It has the following arguments:
#' \itemize{
#'   \item \code{n}, the number of draws (by default \code{n = 1}),
#'   \item \code{mean}, \code{sd}, and \code{df} for the mean, standard 
#'         deviation, and degrees of freedom, respectively (if required), 
#'         in each state,
#'   \item \code{state}, the state number.
#' }
#' The input \code{fixed_parameters} is used to define default arguments.
#' 
#' @examples
#' \dontrun{
#' draw_t_sdd <- build_draw_function("t", list("sd" = 1:2, "df" = 1:2))
#' draw_t_sdd(mean = c(-10, 10), state = 1)
#' draw_t_sdd(mean = c(-10, 10), state = 2)
#' }
#' 
#' @keywords internal  

build_draw_function <- function(class, fixed_parameters) {
  stopifnot(is.character(class), length(class) == 1)
  stopifnot(class %in% c("norm", "t", "gamma", "lnorm", "poisson"))
  stopifnot(is.list(fixed_parameters))
  stopifnot(check_all_list_elements_named(fixed_parameters))
  draw_function <- if (class == "norm") {
    function (n = 1, mean, sd, state) {
      stats::rnorm(n, mean = mean[state], sd = sd[state])
    }
  } else if (class == "t") {
    function (n = 1, mean, sd, df, state) {
      stats::rt(n = n, df[state]) * sd[state] + mean[state]
    }
  } else if (class == "gamma") {
    function (n = 1, mean, sd, state) {
      stats::rgamma(
        n = n, shape = mean[state]^2 / sd[state]^2,
        scale = sd[state]^2 / mean[state]
      )
    } 
  } else if (class == "lnorm") {
    function (n = 1, mean, sd, state) {
      stats::rlnorm(n = n, meanlog = mean[state], sdlog = sd[state])
    }
  } else if (class == "poisson") {
    function (n = 1, mean, state) {
      stats::rpois(n = n, lambda = mean)
    }
  }
  set_default_arguments(draw_function, fixed_parameters)
}

#' Set default arguments to a \code{function}
#'
#' @description 
#' This helper function defines default arguments for a \code{function}.
#' 
#' @param f
#' A \code{function}.
#' @param default_arguments
#' A named \code{list} of default arguments to \code{f}.
#' 
#' @return 
#' The input \code{f} with defined default arguments.
#' 
#' @examples
#' \dontrun{
#' f <- function(x) x
#' f <- set_default_arguments(f, list("x" = 1))
#' f()
#' }
#' 
#' @keywords internal  

set_default_arguments <- function(f, default_arguments = list()) {
  stopifnot(is.function(f))
  stopifnot(is.list(default_arguments))
  stopifnot(names(default_arguments) %in% names(formals(f)))
  formals(f)[names(default_arguments)] <- default_arguments
  return(f)
}

#' Check if all elements of a \code{list} are named
#'
#' @description 
#' This helper function checks whether all elements of a \code{list} are named.
#' 
#' @param x
#' A \code{list}.
#' 
#' @return 
#' Either \code{TRUE} or \code{FALSE}.
#' 
#' @examples
#' \dontrun{
#' check_all_list_elements_named(list())
#' check_all_list_elements_named(list(1))
#' check_all_list_elements_named(list("a" = 1))
#' check_all_list_elements_named(list("a" = 1, 2))
#' check_all_list_elements_named(list("a" = 1, "b" = 2))
#' }
#' 
#' @keywords internal  

check_all_list_elements_named <- function(x) {
  stopifnot(is.list(x))
  length(x) == sum(names(x) != "", na.rm = TRUE)
}


encode_sdd <- function(sdd) {
  sdd_tws <- gsub(" ", "", sdd)
  sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
  distr <- sdd_tws_split[1]
  if (!distr %in% c("norm", "t", "gamma", "lnorm", "poisson")) {
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




