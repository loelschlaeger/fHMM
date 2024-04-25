#' @useDynLib fHMM, .registration=TRUE
#' @keywords internal
"_PACKAGE"

#' @noRd

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0("Thanks for using {fHMM} version ", utils::packageVersion("fHMM")), "!\n",
    "See ", cli::style_hyperlink("https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM") ," for help."
  )
  packageStartupMessage(msg)
  invisible()
}

## usethis namespace: start
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_number
#' @importFrom checkmate expect_number
#' @importFrom checkmate test_atomic_vector
#' @importFrom checkmate test_character
#' @importFrom checkmate test_count
#' @importFrom checkmate test_flag
#' @importFrom checkmate test_int
#' @importFrom checkmate test_integerish
#' @importFrom checkmate test_list
#' @importFrom checkmate test_logical
#' @importFrom checkmate test_number
#' @importFrom checkmate test_numeric
#' @importFrom checkmate test_scalar_na
#' @importFrom checkmate test_string
#' @importFrom cli style_hyperlink
#' @importFrom foreach %dopar%
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics curve
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices rgb
#' @importFrom MASS ginv
#' @importFrom padr pad
#' @importFrom pracma hessian
#' @importFrom Rcpp evalCpp
#' @importFrom stats acf
#' @importFrom stats AIC
#' @importFrom stats BIC
#' @importFrom stats dbinom
#' @importFrom stats dgamma
#' @importFrom stats dnorm
#' @importFrom stats dpois
#' @importFrom stats dt
#' @importFrom stats logLik
#' @importFrom stats na.omit
#' @importFrom stats nlm
#' @importFrom stats nobs
#' @importFrom stats pgamma
#' @importFrom stats pt
#' @importFrom stats qnorm
#' @importFrom stats qqnorm
#' @importFrom stats qunif
#' @importFrom stats rgamma
#' @importFrom stats rlnorm
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @importFrom stats rt
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom utils download.file
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils tail
## usethis namespace: end
NULL
