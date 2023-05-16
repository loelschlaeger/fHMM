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
#' \code{length(sdds)}, i.e., of length 2 in the hierarchical case and 1 else.
#' 
#' Each \code{list} element again is a \code{list}, containing the output of
#' \code{\link{decode_sdd}}.
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
    ### allow for re-definition of already specified 'fHMM_sdds' object
    sdds <- sapply(sdds, function(x) x$label)
  }
  if (!is.character(sdds) || length(sdds) != ifelse(hierarchy, 2, 1)) {
    stop(
      "The control 'sdds' must be a character ", 
      if (hierarchy) "vector ", "of length ", ifelse(hierarchy, 2, 1), ".",
      call. = FALSE
    )
  }
  
  ### define the 'fHMM_sdds' object
  out <- list()
  for (i in if (hierarchy) 1:2 else 1) {
    out[[i]] <- decode_sdd(sdd = sdds[i], state = states[i])
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
#' @param state
#' A single specification for the \code{"states"} control,
#' see \code{\link{set_controls}}.
#' 
#' @return 
#' A \code{list} with the elements
#' \itemize{
#'   \item \code{"distr_class"}, a \code{character} which defines the 
#'   distribution class and can be one of
#'   \itemize{
#'     \item \code{"normal"} (normal distribution),
#'     \item \code{"lognormal"} (log-normal distribution),
#'     \item \code{"t"} (t-distribution),
#'     \item \code{"gamma"} (gamma distribution),
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
#' decode_sdd(sdd = "t(mu = 1)", state = 3)
#' }
#' 
#' @keywords internal

decode_sdd <- function(sdd, state) {
  stopifnot(
    is.character(sdd), length(sdd) == 1, is.numeric(state), length(state) == 1, 
    is_number(state, int = TRUE, pos = TRUE), state >= 2
  )
  sdd_tws <- gsub(" ", "", sdd)
  sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
  distr_class <- sdd_tws_split[1]
  if (!distr_class %in% c("normal", "lognormal", "t", "gamma", "poisson")) {
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
  if (distr_class %in% c("normal", "lognormal", "gamma")) {
    fixed_pars <- check_par_names_allowed(fixed_pars, c("mu", "sigma"))
  }
  if (distr_class == "poisson") {
    fixed_pars <- check_par_names_allowed(fixed_pars, "mu")
  }
  if ("mu" %in% names(fixed_pars)) {
    if (distr_class %in% c("gamma", "poisson")) {
      if (!any(is_number(fixed_pars$mu, pos = TRUE))) {
        stop("'mu' must be positive.", call. = FALSE)
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
      fixed_pars[[p]] <- rep(fixed_pars[[p]], state)
    }
    if (length(fixed_pars[[p]]) != state) {
      stop(
        "Number of fixed parameters in '", label, 
        "' does not fit the number of states (", state, ").",
        call. = FALSE
      )
    }
  }
  list(
    "distr_class"  = distr_class,
    "name"         = distr_class, # TODO: REMOVE LATER
    "label"        = label,
    "fixed_pars"   = fixed_pars,
    "pars"         = fixed_pars,  # TODO: REMOVE LATER
    "sample"       = build_sdd_function(distr_class, "sample"),
    "density"      = build_sdd_function(distr_class, "density"),
    "distribution" = build_sdd_function(distr_class, "distribution")
  )
}

#' Build sampling, density, or distribution function for state-dependent 
#' distributions
#'
#' @description 
#' This helper function builds a \code{function} for sampling, density 
#' computation, or cumulative distribution for a state-dependent distribution.
#' 
#' @param distr_class
#' A \code{character} which defines the class of the state-dependent 
#' distribution, see the output of \code{\link{decode_sdd}}.
#' @param function_type
#' A \code{character} which defines the desired function type and can be one of
#' \itemize{
#'   \item \code{"sample"} (sampling function),
#'   \item \code{"density"} (density function),
#'   \item \code{"distribution"} (distribution function).
#' }
#' 
#' @return 
#' A \code{function} which either returns 
#' \itemize{
#'   \item random samples if \code{function_type = "sample"},
#'   \item the density if \code{function_type = "density"},
#'   \item the cumulative distribution if \code{function_type = "distribution"},
#' }
#' from the state-dependent distribution specified as \code{distr_class}.
#'  
#' The returned \code{function} has the arguments
#' \itemize{
#'   \item either \itemize{
#'     \item \code{n}, the number of samples (by default \code{n = 1}),
#'     \item \code{x}, the point where to compute the density,
#'     \item \code{q}, the point where to compute the cumulative distribution,
#'   }
#'   \item \code{state}, the state index,
#'   \item \code{...}, vectors with parameters for each state, named
#'         \code{mu}, \code{sigma}, and \code{df}, for the mean, 
#'         standard deviation, and degrees of freedom, respectively 
#'         (if required).
#' }
#' 
#' @examples
#' \dontrun{
#' sample_poisson_sdd <- build_sdd_function(
#'   distr_class = "poisson", function_type = "sample"
#' )
#' sample_poisson_sdd(n = 10, state = 1, mu = c(1, 10))
#' sample_poisson_sdd(n = 10, state = 2, mu = c(1, 10))
#' }
#' 
#' @keywords internal  

build_sdd_function <- function(distr_class, function_type) {
  stopifnot(
    is.character(distr_class), length(distr_class) == 1,
    distr_class %in% c("normal", "lognormal", "t", "gamma", "poisson"),
    is.character(function_type), length(function_type) == 1,
    function_type %in% c("sample", "density", "distribution")
  )
  sdd_function <- function () {}
  formals(sdd_function) <- if (function_type == "sample") {
    alist(n = 1, state =, ... =)
  } else if (function_type == "density") {
    alist(x =, state =, ... =)
  } else if (function_type == "distribution") {
    alist(q =, state =, ... =)
  }
  body(sdd_function)[2] <- as.expression(as.call(quote(
    if (missing(state)) {
      stop(
        "Which state? Please set, e.g., 'state' = 1.", 
        call. = FALSE
      )
    }
  )))
  body(sdd_function)[3] <- as.expression(as.call(quote(
    args <- list(...)
  )))
  body(sdd_function)[4] <- if (distr_class %in% c("normal", "lognormal", "gamma")) {
    as.expression(as.call(quote(
      stopifnot(
        "'mu' must be specified" = "mu" %in% names(args),
        "'sigma' must be specified" = "sigma" %in% names(args)
      )
    )))
  } else if (distr_class == "t") {
    as.expression(as.call(quote(
      stopifnot(
        "'mu' must be specified" = "mu" %in% names(args),
        "'sigma' must be specified" = "sigma" %in% names(args),
        "'df' must be specified" = "df" %in% names(args)
      )
    )))
  } else if (distr_class == "poisson") {
    as.expression(as.call(quote(
      stopifnot(
        "'mu' must be specified" = "mu" %in% names(args)
      )
    )))
  }
  body(sdd_function)[5] <- if (function_type == "sample") {
    if (distr_class == "normal") {
      as.expression(as.call(quote(
        stats::rnorm(n, mean = args$mu[state], sd = args$sigma[state])
      )))
    } else if (distr_class == "lognormal") {
      as.expression(as.call(quote(
        stats::rlnorm(n = n, meanlog = args$mu[state], sdlog = args$sigma[state])
      )))
    } else if (distr_class == "t") {
      as.expression(as.call(quote(
        stats::rt(n = n, args$df[state]) * args$sigma[state] + args$mu[state]
      )))
    } else if (distr_class == "gamma") {
      as.expression(as.call(quote(
        stats::rgamma(
          n = n, shape = args$mu[state]^2 / args$sigma[state]^2,
          scale = args$sigma[state]^2 / args$mu[state]
        )
      )))
    } else if (distr_class == "poisson") {
      as.expression(as.call(quote(
        stats::rpois(n = n, lambda = args$mu[state])
      )))
    }
  } else if (function_type == "density") {
    if (distr_class == "normal") {
      as.expression(as.call(quote(
        stats::dnorm(
          x = x, 
          mean = args$mu[state], 
          sd = args$sigma[state]
        )
      )))
    } else if (distr_class == "lognormal") {
      as.expression(as.call(quote(
        stats::dlnorm(
          x = x, 
          meanlog = args$mu[state], 
          sdlog = args$sigma[state]
        )
      )))
    } else if (distr_class == "t") {
      as.expression(as.call(quote(
        1 / args$sigma[state] * stats::dt(
          x = (x - args$mu[state]) / args$sigma[state],
          df = args$df[state]
        )
      )))
    } else if (distr_class == "gamma") {
      as.expression(as.call(quote(
        stats::dgamma(
          x = x,
          shape = args$mu[state]^2 / args$sigma[state]^2,
          scale = args$sigma[state]^2 / args$mu[state]
        )
      )))
    } else if (distr_class == "poisson") {
      as.expression(as.call(quote(
        stats::dpois(
          x = x,
          mean = args$mu[state]
        )
      )))
    }
  } else if (function_type == "distribution") {
    if (distr_class == "normal") {
      as.expression(as.call(quote(
        stats::pnorm(
          q = q,
          mean = args$mu[state],
          sd = args$sigma[state]
        )
      )))
    } else if (distr_class == "lognormal") {
      as.expression(as.call(quote(
        stats::plnorm(
          q = q,
          meanlog = args$mu[state],
          sdlog = args$sigma[state]
        )
      )))
    } else if (distr_class == "t") {
      as.expression(as.call(quote(
        stats::pt(
          q = (q - args$mu[state]) / args$sigma[state],
          df = arg$df[state]
        )
      )))
    } else if (distr_class == "gamma") {
      as.expression(as.call(quote(
        stats::pgamma(
          q = q,
          shape = args$mu[state]^2 / args$sigma[state]^2,
          scale = args$sigma[state]^2 / args$mu[state]
        )
      )))
    } else if (distr_class == "poisson") {
      as.expression(as.call(quote(
        stats::ppois(
          q = q,
          mean = args$mu[state]
        )
      )))
    }
  }
  return(sdd_function)
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
    cat(
      "coarse-scale: ", x[[1]]$label, 
      ", fine-scale: ", x[[2]]$label, 
      sep = ""
    )
  }
  return(invisible(x))
}

