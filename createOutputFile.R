createOutputFile <- function(est,fileName="output.txt"){
  est_gammas <- est[1:((M-1)*M+M*(N-1)*N)]; est <- est[-(1:((M-1)*M+M*(N-1)*N))]
  Gamma <- gammasCon2Gamma(est_gammas[1:((M-1)*M)],M)
  est_gammas <- est_gammas[-(1:((M-1)*M))]
  Gammas_star <- list()
  for(i in 1:M){
    Gammas_star[[i]] <- gammasCon2Gamma(est_gammas[1:((N-1)*N)],N); est_gammas <- est_gammas[-(1:((N-1)*N))]
  }
  
  est_mus <- est[1:(M+M*N)]; est <- est[-(1:(M+M*N))]
  mu <- est_mus[1:M]
  est_mus <- est_mus[-(1:M)]
  mus_star <- list()
  for(i in 1:M){
    mus_star[[i]] <- est_mus[1:N]; est_mus <- est_mus[-(1:N)]
  }
  
  est_sigmas <- est[1:(M+M*N)]; est <- est[-(1:(M+M*N))]
  sigma <- est_sigmas[1:M]
  est_sigmas <- est_sigmas[-(1:M)]
  sigmas_star <- list()
  for(i in 1:M){
    sigmas_star[[i]] <- est_sigmas[1:N]; est_sigmas <- est_sigmas[-(1:N)]
  }
  
  est_dfs <- est[1:(M+M*N)]; est <- est[-(1:(M+M*N))]
  df <- est_dfs[1:M]
  est_dfs <- est_dfs[-(1:M)]
  dfs_star <- list()
  for(i in 1:M){
    dfs_star[[i]] <- est_dfs[1:N]; est_dfs <- est_dfs[-(1:N)]
  }
  
  fileConnection <- file("fileName")
  text <- paste("Estimation with cs states M =",M,", fs states N =",N,", number of runs =",runs,".")
  text <- c(text,"Log-Likelihood at Maximum",-mod$minimum)
  text <- c(text,"Parameter on the cs:")
  text <- c(text,"Gamma",as.character(Gamma))
  text <- c(text,"mu",as.character(mu))
  text <- c(text,"sigma",as.character(sigma))
  text <- c(text,"df",as.character(df))
  text <- c(text,"Parameter on the fs:")
  for(i in 1:M){
    text <- c(text,paste("cs state is ",as.character(i)))
    text <- c(text,"Gamma",as.character(Gammas_star[[i]]))
    text <- c(text,"mu",as.character(mus_star[[i]]))
    text <- c(text,"sigma",as.character(sigmas_star[[i]]))
    text <- c(text,"df",as.character(dfs_star[[i]]))
  }
  writeLines(text,fileConnection)
  close(fileConnection)
}