// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// LL_HMM_Rcpp
double LL_HMM_Rcpp(arma::mat allprobs, arma::mat Gamma, arma::vec delta, int N, int T);
RcppExport SEXP _fHMM_LL_HMM_Rcpp(SEXP allprobsSEXP, SEXP GammaSEXP, SEXP deltaSEXP, SEXP NSEXP, SEXP TSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allprobs(allprobsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Gamma(GammaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type T(TSEXP);
    rcpp_result_gen = Rcpp::wrap(LL_HMM_Rcpp(allprobs, Gamma, delta, N, T));
    return rcpp_result_gen;
END_RCPP
}
// LL_HHMM_Rcpp
double LL_HHMM_Rcpp(arma::mat log_likelihoods, arma::mat allprobs, arma::mat Gamma, arma::vec delta, int M, int T);
RcppExport SEXP _fHMM_LL_HHMM_Rcpp(SEXP log_likelihoodsSEXP, SEXP allprobsSEXP, SEXP GammaSEXP, SEXP deltaSEXP, SEXP MSEXP, SEXP TSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type log_likelihoods(log_likelihoodsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type allprobs(allprobsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Gamma(GammaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type T(TSEXP);
    rcpp_result_gen = Rcpp::wrap(LL_HHMM_Rcpp(log_likelihoods, allprobs, Gamma, delta, M, T));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fHMM_LL_HMM_Rcpp", (DL_FUNC) &_fHMM_LL_HMM_Rcpp, 5},
    {"_fHMM_LL_HHMM_Rcpp", (DL_FUNC) &_fHMM_LL_HHMM_Rcpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_fHMM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
