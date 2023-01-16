#' Compare multiple models
#'
#' @description
#' This function performs model comparison by comparing multiple 
#' \code{\link{fHMM_model}} objects with respect to
#' \itemize{
#'   \item the number of model parameters,
#'   \item the log-likelihood value,
#'   \item the AIC value,
#'   \item the BIC value.
#' }
#'
#' @param ...
#' A list of one or more objects of class \code{\link{fHMM_model}}.
#'
#' @return
#' A \code{data.frame} with models in rows and comparison criteria in columns.
#'
#' @examples
#' ### 3-state HMM with t-distributions is preferred over 2-state HMM with
#' ### normal distributions for the DAX data based on AIC and BIC
#' compare_models(dax_model_2n, dax_model_3t)
#' 
#' @export

compare_models <- function(...) {

  ### read models
  models <- as.list(list(...))

  ### get model names
  model_names <- unlist(lapply(sys.call()[-1], as.character))

  ### check if models are of class "fHMM_model"
  for (i in seq_len(length(models))) {
    if (!inherits(models[[i]],"fHMM_model")) {
      stop(
        paste0("Input '", model_names[i], "' is not of class 'fHMM_model'."),
        call. = FALSE
      )
    }
  }

  ### check if data is the same for each model
  for (i in seq_len(length(models))) {
    data_i <- as.numeric(unlist(models[[i]]$data$data))
    for (j in 1:i) {
      data_j <- as.numeric(unlist(models[[j]]$data$data))
      if (!identical(data_i, data_j)) {
        warning(
          paste0(
            "Models '", model_names[i], "' and '", model_names[j], 
            "' are not estimated on the same data, be cautious comparing them."
          ),
          call. = FALSE
        )
      }
    }
  }

  ### create output matrix
  criteria <- c("parameters", "loglikelihood", "AIC", "BIC")
  output <- matrix(NA_real_, nrow = length(models), ncol = length(criteria))
  rownames(output) <- model_names
  colnames(output) <- criteria
  for (i in seq_len(length(models))) {
    summary_i <- summary(models[[i]])
    output[i, "parameters"] <- summary_i$no_par
    output[i, "loglikelihood"] <- summary_i$model_info$LL
    output[i, "AIC"] <- summary_i$model_info$AIC
    output[i, "BIC"] <- summary_i$model_info$BIC
  }

  ### return output
  as.data.frame(output)
}
