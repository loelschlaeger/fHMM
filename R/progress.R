#' Print progess.
#' @description
#' This function prints the progress of some loop.
#' @param run
#' The current iteration. Set \code{run = 0} for an initial message.
#' @param total_runs
#' The total number of iterations.
#' @param start_time
#' The time at which the loop started.
#' @param message
#' A character, a message printed in front of the progress bar.
#' The initial message.
#' @return
#' No return value, only on-screen information.
#' @example
#' start_time = Sys.time()
#' total_runs = 10
#' progress(run = 0, total_runs = total_runs, start_time = start_time,
#'          message = "Progress:")
#' for(run in 1:total_runs){
#'   Sys.sleep(0.5)
#'   progress(run = run, total_runs = total_runs, start_time = start_time,
#'            message = "Progress:")
#' }
#' @export

progress = function(run, total_runs, start_time, message = NULL) {
  
  ### input checks
  if(run > total_runs)
    stop("'run' must be smaller than 'total_runs'.")
  
  ### compute estimated remaining time 'eta'
  current_time = Sys.time()
  time_till_now = difftime(current_time, start_time)
  time_per_run = time_till_now / run
  eta = as.difftime(time_per_run * (total_runs - run), units = "secs")
  
  ### change unit of 'eta'
  if(run == 0){
    eta = ""
  } else if(run == total_runs) {
    eta = "\n"
  } else {
    if(eta > 99){
      units(eta) = "mins"
      if(eta > 99)
        units(eta) = "hours"
    }
    eta = paste(sprintf("%2.0f", eta), units(eta),"left")
  }
  
  ### create progress bar
  extra = nchar('||100%')
  length_time = length(eta)
  width = max(26, extra + length_time)
  step = round(run / total_runs * (width - extra - length_time))
  empty = width - extra - length_time - step
  text = sprintf("\r%s |%s%s|% 3s%% %s", 
                 message,
                 paste0(strrep('=', max(step,0)),">"),
                 strrep('-', empty), 
                 round(run / total_runs * 100),
                 eta)
  cat(text)
}
