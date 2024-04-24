#' Set color scheme for visualizations
#'
#' @description
#' This helper function defines a color scheme for visualizations in the 
#' \{fHMM\} package.
#'
#' @param controls
#' An object of class \code{fHMM_controls}.
#' It can be created with \code{\link{set_controls}}.
#' @param colors
#' Either \code{NULL} (default) or a \code{character} vector of color names or 
#' hexadecimal RGB triplets.
#'
#' @return
#' An object of class \code{fHMM_colors}, which is:
#' \itemize{
#'   \item for \code{controls$hierarchy == FALSE} a \code{character} vector of 
#'         length \code{controls$states} of color codes,
#'   \item for \code{controls$hierarchy == TRUE} a \code{list} of
#'         \itemize{
#'           \item a \code{character} vector of length  \code{controls$states[1]} and
#'           \item a \code{character} matrix of dimensions \code{controls$states}
#'         }
#'         with color codes.
#' }
#'
#' @examples
#' \dontrun{
#' controls <- set_controls()
#' fHMM_colors(controls, colors = c("red", "blue"))
#' }
#' 
#' @keywords internal

fHMM_colors <- function(controls, colors = NULL) {

  ### check inputs
  if (!inherits(controls,"fHMM_controls")) {
    stop("'controls' must be of class 'fHMM_controls'.", call. = FALSE)
  }
  if (is.null(colors)) {
    colors <- c("darkred", "red", "orange", "yellow", "green", "darkgreen")
  }
  if (!is.character(colors)) {
    stop("'colors' must be a character vector.", call. = FALSE)
  }
  for (col in colors) {
    out <- tryCatch(is.matrix(grDevices::col2rgb(col)), error = function(e) FALSE)
    if (out == FALSE) {
      stop("'", col, "' in 'colors' is not a valid color representation.",
           call. = FALSE)
    }
  }

  ### helper functions
  var_col <- function(col, n) grDevices::colorRampPalette(c("white", col, "black"))(n + 2)[2:(n + 1)]
  base_col <- function(n) grDevices::colorRampPalette(colors)(n)
  col_alpha <- function(col, alpha = 0.6) grDevices::adjustcolor(col, alpha)

  ### create and return 'fHMM_colors'
  if (!controls[["hierarchy"]]) {
    out <- col_alpha(base_col(controls[["states"]][1]))
  } else {
    out <- matrix(NA_character_,
      nrow = controls[["states"]][1],
      ncol = controls[["states"]][2] + 1
    )
    out[, 1] <- col_alpha(base_col(controls[["states"]][1]))
    for (s in seq_len(controls[["states"]][1])) {
      out[s, -1] <- col_alpha(var_col(out[s, 1], controls[["states"]][2]))
    }
    out <- list("cs" = out[, 1], fs = out[, -1])
  }
  class(out) <- "fHMM_colors"
  return(out)
}
