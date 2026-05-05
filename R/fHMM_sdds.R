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
  oeli::input_check_response(
    check = if (checkmate::test_atomic_vector(states)) {
      TRUE
    } else {
      "The control 'states' must be a vector."
    },
    var_name = "states"
  )
  if (length(states) == 1) {
    hierarchy <- FALSE
    oeli::input_check_response(
      check = if (checkmate::test_integerish(states, lower = 2, len = 1)) {
        TRUE
      } else {
        "The control 'states' must be an integer greater or equal 2."
      },
      var_name = "states"
    )
  } else if (length(states) == 2) {
    hierarchy <- TRUE
    oeli::input_check_response(
      check = if (checkmate::test_integerish(states, lower = 2, len = 2)) {
        TRUE
      } else {
        "The control 'states' must be a vector of integers greater or equal 2."
      },
      var_name = "states"
    )
  } else {
    oeli::input_check_response(
      check = "The control 'states' must be a vector of length 1 or 2.",
      var_name = "states"
    )
  }
  oeli::input_check_response(
    check = if (
      checkmate::test_character(
        sdds, any.missing = FALSE, len = ifelse(hierarchy, 2, 1)
      )
    ) {
      TRUE
    } else {
      paste0(
        "The control 'sdds' must be a character ",
        if (hierarchy) "vector of length " else "of length ",
        ifelse(hierarchy, 2, 1), "."
      )
    },
    var_name = "sdds"
  )
  
  ### decode state-dependent distribution specification
  out <- list()
  for (i in if (hierarchy) 1:2 else 1) {
    sdd <- sdds[i]
    oeli::input_check_response(
      check = checkmate::check_string(sdd),
      var_name = "sdds"
    )
    sdd_tws <- gsub(" ", "", sdd)
    sdd_tws_split <- unlist(strsplit(sdd_tws, split = "[()]"))
    distr <- sdd_tws_split[1]
    oeli::input_check_response(
      check = if (
        distr %in% c("normal", "lognormal", "t", "gamma", "poisson")
      ) {
        TRUE
      } else {
        paste0(
          "Currently, only the following distributions are implemented:\n",
          "- normal distribution ('normal')\n",
          "- log-normal distribution ('lognormal')\n",
          "- t-distribution ('t')\n",
          "- Gamma distribution ('gamma')\n",
          "- Poisson distribution ('poisson')"
        )
      },
      var_name = "sdds"
    )
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
      oeli::input_check_response(
        check = if (length(pars[[par]]) %in% c(1, states[i])) {
          TRUE
        } else {
          paste0(
            "Fixed values for the parameter '", par,
            "' must be of length 1 or ", states[i], "."
          )
        },
        var_name = par
      )
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
        oeli::input_check_response(
          check = if (checkmate::test_numeric(pars$mu) && all(pars$mu > 0)) {
            TRUE
          } else {
            "'mu' must be a positive numeric."
          },
          var_name = "mu"
        )
      }
    } else {
      pars$mu <- NULL
    }
    if (!is.null(pars$sigma)) {
      oeli::input_check_response(
        check = if (
          checkmate::test_numeric(pars$sigma) && all(pars$sigma > 0)
        ) {
          TRUE
        } else {
          "'sigma' must be a positive numeric."
        },
        var_name = "sigma"
      )
    } else {
      pars$sigma <- NULL
    }
    if (!is.null(pars$df)) {
      oeli::input_check_response(
        check = if (checkmate::test_numeric(pars$df) && all(pars$df > 0)) {
          TRUE
        } else {
          "'df' must be a positive numeric."
        },
        var_name = "df"
      )
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
