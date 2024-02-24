// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include<iostream>
#include<cmath>

// [[Rcpp::export]]

double LL_HMM_Rcpp(
    arma::mat allprobs, 
    arma::mat Gamma, 
    arma::vec delta, 
    int N, 
    int T
  ) {
  
  int i = 0;
	int t = 1;
	double c = 0.0;
	arma::mat phi = arma::mat(N, T);
	
	for (i = 0; i < N; i++) {
		phi(i, 0) = log(delta(i) * allprobs(i, 0));
	}
	
	for (t = 1; t < T; t++) {
	
	  // 'c' is a neutral constant for numerical stability
		c = max(phi.col(t - 1));
		for (i = 0; i < N; i++) {
			phi(i, t) = log(sum(exp(phi.col(t - 1) + log(Gamma.col(i)) - c))) + c + log(allprobs(i,t));
		}
		
	}
	
	// 'c' is a neutral constant for numerical stability
	c = max(phi.col(T-1));
  return log(sum(exp(phi.col(T - 1) - c))) + c;
  
}

// [[Rcpp::export]]

double LL_HHMM_Rcpp(
    arma::mat log_likelihoods, 
    arma::mat allprobs, 
    arma::mat Gamma, 
    arma::vec delta, 
    int M, 
    int T
  ) {
  
  int i = 0;
  int t = 1;
  double c = 0.0;
  arma::mat phi = arma::mat(M, T);
  
  for (i = 0; i < M; i++) {
    phi(i, 0) = log(delta(i)) + log_likelihoods(i, 0) + log(allprobs(i, 0));
  }
  
  for (t = 1; t < T; t++) {
    
    // 'c' is a neutral constant for numerical stability
    c = max(phi.col(t - 1));
    for (i = 0; i < M; i++) {
      phi(i, t) = log(sum(exp(phi.col(t - 1) + log(Gamma.col(i)) - c))) + c + log_likelihoods(i, t) + log(allprobs(i, t));
    }
    
  }
  
  // 'c' is a neutral constant for numerical stability
  c = max(phi.col(T - 1));
  return log(sum(exp(phi.col(T - 1) - c))) + c;
  
}
