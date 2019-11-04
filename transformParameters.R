# INPUT: tpm
# OUTPUT: non-diagonal elements, column-wise
Gamma2gammas <- function(Gamma){
	g <- Gamma
	diag(g) <- NA
	return(g[!is.na(g)])
}

# INPUT: unconstrained non-diagonal elements of tpm
# OUTPUT: tpm
gammasUncon2Gamma <- function(gammas,N){
	g <- diag(N)
	g[!g] <- exp(gammas) # filled column-wise
	g <- g/rowSums(g)
	return(g)
}

# INPUT: constrained non-diagonal elements of tpm
# OUTPUT: tpm
gammasCon2Gamma <- function(gammas,N){
	g <- diag(N)
	g[!g] <- gammas
	for(i in 1:N){
		g[i,i] <- 1-(rowSums(g)[i]-1)
	}
	return(g)
}

# INPUT: unconstrained estimates
# OUTPUT: constrained estimates
backTransform <- function(theta,controls){
  
  M      = controls[["M"]]
  N      = controls[["N"]]
  est_df = controls[["est_df"]]
  
	g = theta[1:((M-1)*M)]; theta = theta[-(1:((M-1)*M))]
	G = gammasUncon2Gamma(g,M)
	g = Gamma2gammas(G)
	gammas <- c(g)
	for(i in 1:M){
		g      = theta[1:((N-1)*N)]; theta = theta[-(1:((N-1)*N))]
		G      = gammasUncon2Gamma(g,N)
		g      = Gamma2gammas(G)
		gammas = c(gammas,g)
	}
	mus     = theta[1:(M+M*N)]; theta = theta[-(1:(M+M*N))]
	sigmas  = theta[1:(M+M*N)]; theta = theta[-(1:(M+M*N))]
	sigmas  = exp(sigmas)
	if(est_df=="all") {dfs     = theta[1:(M+M*N)]; dfs = round(dfs*30)+1; theta = theta[-(1:(M+M*N))]} 
	if(est_df=="fscs"){dfs     = theta[1:2];       dfs = round(dfs*30)+1; theta = theta[-(1:2)]      }
	if(est_df=="no")  {dfs     = c(controls[["set_df_cs"]],controls[["set_df_fs"]]) }
	
	output = list(
	  "gammas" = gammas,
	  "mus"    = mus,
	  "sigmas" = sigmas,
	  "dfs"    = dfs
	)
	
	return(output)
}
