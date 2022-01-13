#' Model fitting for the fHMM package.
#' @description
#' This function fits an (hierarchical) HMM for the fHMM package.
#' @param data
#' An object of class \code{fHMM_data}.
#' @param seed
#' Set a seed for the sampling of initial values.
#' @param ncluster
#' Set the number of clusters for parallelization.
#' @return
#' An object of class \code{fHMM_model}, which is a list of
#' \itemize{
#'   \item ...
#' }
#' @export
#' @importFrom stats sd nlm
#' @importFrom foreach %dopar%

fit_model <- function(data, ncluster = 1, seed = NULL) {

  ### check inputs
  if (class(data) != "fHMM_data") {
    stop("'data' is not of class 'fHMM_data'.")
  }
  if (!is_number(ncluster, int = TRUE, pos = TRUE)) {
    stop("'ncluster' must be a positive integer.")
  }

  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### generate start values
  start_values <- list()
  if (data[["controls"]][["fit"]][["origin"]]) {
    start_values[[1]] <- par2parUncon(data[["true_parameters"]], data[["controls"]])
  } else {
    ### compute parameter scales based on the method of moments
    scale_par <- c(1, 1)
    if (!data[["controls"]][["hierarchy"]]) {
      scale_par[1] <- mean(c(mean(data[["data"]], na.rm = "TRUE"), stats::sd(data[["data"]], na.rm = "TRUE")))
    } else {
      scale_par[1] <- mean(c(mean(data[["data"]][, 1], na.rm = "TRUE"), stats::sd(data[["data"]][, 1], na.rm = "TRUE")))
      scale_par[2] <- mean(c(mean(data[["data"]][, -1], na.rm = "TRUE"), stats::sd(data[["data"]][, -1], na.rm = "TRUE")))
    }
    scale_par <- abs(scale_par)
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      start_values[[run]] <- par2parUncon(
        fHMM_parameters(controls = data[["controls"]], scale_par = scale_par),
        data[["controls"]]
      )
    }
  }

  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)

  ### check start values
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent, :eta ETA",
    total = data[["controls"]][["fit"]][["runs"]], width = 45, clear = TRUE,
    complete = "=", incomplete = "-", current = ">"
  )
  pb$message("Checking start values")
  ll_at_start_values <- rep(NA, data[["controls"]][["fit"]][["runs"]])
  if (ncluster == 1) {
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      pb$tick(0)
      ll <- target(
        parUncon = start_values[[run]],
        observations = data[["data"]],
        controls = data[["controls"]]
      )
      if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
        ll_at_start_values[run] <- ll
      }
      pb$tick()
    }
  } else if (ncluster > 1) {
    numCores <- parallel::detectCores()
    cluster <- parallel::makeCluster(ncluster)
    doSNOW::registerDoSNOW(cluster)
    opts <- list(progress = function(n) pb$tick())
    ll_at_start_values <- foreach::foreach(
      run = 1:data[["controls"]][["fit"]][["runs"]],
      .packages = "fHMM", .options.snow = opts
    ) %dopar% {
      pb$tick(0)
      ll <- target(
        parUncon = start_values[[run]],
        observations = data[["data"]],
        controls = data[["controls"]]
      )
      if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
        ll
      } else {
        NA
      }
      pb$tick()
    }
    parallel::stopCluster(cluster)
    ll_at_start_values <- unlist(ll_at_start_values)
  }
  if (sum(is.na(ll_at_start_values)) == data[["controls"]][["fit"]][["runs"]]) {
    stop("F.2")
  }
  if (sum(is.na(ll_at_start_values)) > 0.5 * data[["controls"]][["fit"]][["runs"]]) {
    warning("F.3", immediate. = TRUE)
  }
  runs_seq <- which(!is.na(ll_at_start_values))

  ### start optimization
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent, :eta ETA",
    total = data[["controls"]][["fit"]][["runs"]], width = 45, clear = TRUE,
    complete = "=", incomplete = "-", current = ">"
  )
  pb$message("Maximizing likelihood")
  start_time <- Sys.time()
  if (ncluster == 1) {
    mods <- list()
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      pb$tick(0)
      if (!is.na(ll_at_start_values[run])) {
        suppressWarnings({
          mod <- try(
            stats::nlm(
              f = target,
              p = start_values[[run]],
              observations = data[["data"]],
              controls = data[["controls"]],
              iterlim = data[["controls"]][["fit"]][["iterlim"]],
              steptol = data[["controls"]][["fit"]][["steptol"]],
              gradtol = data[["controls"]][["fit"]][["gradtol"]],
              print.level = data[["controls"]][["fit"]][["print.level"]],
              typsize = start_values[[run]],
              hessian = FALSE
            ),
            silent = TRUE
          )
        })
      } else {
        mod <- NA
      }
      if (!is.na(mod) && class(mod) != "try-error" &&
        mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]) {
        mods[[run]] <- mod
      } else {
        mods[[run]] <- NA
      }
      pb$tick()
    }
  } else if (ncluster > 1) {
    numCores <- parallel::detectCores()
    cluster <- parallel::makeCluster(ncluster)
    doSNOW::registerDoSNOW(cluster)
    opts <- list(progress = function(n) pb$tick())
    mods <- foreach::foreach(
      run = 1:data[["controls"]][["fit"]][["runs"]],
      .packages = "fHMM", .options.snow = opts
    ) %dopar% {
      pb$tick(0)
      if (!is.na(ll_at_start_values[run])) {
        suppressWarnings({
          mod <- try(
            stats::nlm(
              f = target,
              p = start_values[[run]],
              observations = data[["data"]],
              controls = data[["controls"]],
              iterlim = data[["controls"]][["fit"]][["iterlim"]],
              steptol = data[["controls"]][["fit"]][["steptol"]],
              gradtol = data[["controls"]][["fit"]][["gradtol"]],
              print.level = data[["controls"]][["fit"]][["print.level"]],
              typsize = start_values[[run]],
              hessian = FALSE
            ),
            silent = TRUE
          )
        })
        if (class(mod) != "try-error" &&
          mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]) {
          mod
        } else {
          NA
        }
      } else {
        NA
      }
    }
    parallel::stopCluster(cluster)
  }
  end_time <- Sys.time()

  ### save and check likelihood values
  lls <- -unlist(sapply(mods, `[`, "minimum"), use.names = FALSE)
  if (all(is.na(lls))) {
    stop("F.4")
  }

  ### compute Hessian
  message("Computing Hessian")
  hessian <- suppressWarnings(
    stats::nlm(
      f = target,
      p = mods[[which.max(lls)]][["estimate"]],
      observations = data[["data"]],
      controls = data[["controls"]],
      iterlim = 1,
      hessian = TRUE,
      typsize = mods[[which.max(lls)]][["estimate"]]
    )[["hessian"]]
  )

  ### final message
  message("Fitting completed")

  ### extract estimation results
  mod <- mods[[which.max(lls)]]
  ll <- -mod[["minimum"]]
  estimate <- mod[["estimate"]]
  class(estimate) <- "parUncon"
  estimation_time <- ceiling(difftime(end_time, start_time, units = "mins"))

  ### create and return 'fHMM_model' object
  out <- list(
    "data" = data,
    "estimate" = estimate,
    "nlm_output" = mod,
    "estimation_time" = estimation_time,
    "ll" = ll,
    "lls" = lls,
    "gradient" = mod$gradient,
    "hessian" = hessian,
    "decoding" = NULL
  )
  class(out) <- "fHMM_model"
  return(out)
}
