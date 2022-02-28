// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/LambertW.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// W_Cpp
NumericVector W_Cpp(const NumericVector& z, int branch);
static SEXP _LambertW_W_Cpp_try(SEXP zSEXP, SEXP branchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< int >::type branch(branchSEXP);
    rcpp_result_gen = Rcpp::wrap(W_Cpp(z, branch));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _LambertW_W_Cpp(SEXP zSEXP, SEXP branchSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_LambertW_W_Cpp_try(zSEXP, branchSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// W_delta_Cpp
NumericVector W_delta_Cpp(const NumericVector& z, double delta);
static SEXP _LambertW_W_delta_Cpp_try(SEXP zSEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(W_delta_Cpp(z, delta));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _LambertW_W_delta_Cpp(SEXP zSEXP, SEXP deltaSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_LambertW_W_delta_Cpp_try(zSEXP, deltaSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// W_delta_alpha_Cpp
NumericVector W_delta_alpha_Cpp(const NumericVector& z, double delta, double alpha);
static SEXP _LambertW_W_delta_alpha_Cpp_try(SEXP zSEXP, SEXP deltaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(W_delta_alpha_Cpp(z, delta, alpha));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _LambertW_W_delta_alpha_Cpp(SEXP zSEXP, SEXP deltaSEXP, SEXP alphaSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_LambertW_W_delta_alpha_Cpp_try(zSEXP, deltaSEXP, alphaSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// W_gamma_Cpp
NumericVector W_gamma_Cpp(const NumericVector& z, double gamma, int branch);
static SEXP _LambertW_W_gamma_Cpp_try(SEXP zSEXP, SEXP gammaSEXP, SEXP branchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< int >::type branch(branchSEXP);
    rcpp_result_gen = Rcpp::wrap(W_gamma_Cpp(z, gamma, branch));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _LambertW_W_gamma_Cpp(SEXP zSEXP, SEXP gammaSEXP, SEXP branchSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_LambertW_W_gamma_Cpp_try(zSEXP, gammaSEXP, branchSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// kurtosis
double kurtosis(const NumericVector& x);
RcppExport SEXP _LambertW_kurtosis(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(kurtosis(x));
    return rcpp_result_gen;
END_RCPP
}
// lp_norm_Cpp
double lp_norm_Cpp(const NumericVector& x, double p);
RcppExport SEXP _LambertW_lp_norm_Cpp(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(lp_norm_Cpp(x, p));
    return rcpp_result_gen;
END_RCPP
}
// lp_norm_complex_Cpp
double lp_norm_complex_Cpp(const ComplexVector& x, double p);
RcppExport SEXP _LambertW_lp_norm_complex_Cpp(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const ComplexVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(lp_norm_complex_Cpp(x, p));
    return rcpp_result_gen;
END_RCPP
}
// normalize_by_tau_Cpp
NumericVector normalize_by_tau_Cpp(const NumericVector& x, double mu_x, double sigma_x, bool inverse);
RcppExport SEXP _LambertW_normalize_by_tau_Cpp(SEXP xSEXP, SEXP mu_xSEXP, SEXP sigma_xSEXP, SEXP inverseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu_x(mu_xSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_x(sigma_xSEXP);
    Rcpp::traits::input_parameter< bool >::type inverse(inverseSEXP);
    rcpp_result_gen = Rcpp::wrap(normalize_by_tau_Cpp(x, mu_x, sigma_x, inverse));
    return rcpp_result_gen;
END_RCPP
}
// skewness
double skewness(const NumericVector& x);
RcppExport SEXP _LambertW_skewness(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(skewness(x));
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _LambertW_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("NumericVector(*W_Cpp)(const NumericVector&,int)");
        signatures.insert("NumericVector(*W_delta_Cpp)(const NumericVector&,double)");
        signatures.insert("NumericVector(*W_delta_alpha_Cpp)(const NumericVector&,double,double)");
        signatures.insert("NumericVector(*W_gamma_Cpp)(const NumericVector&,double,int)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _LambertW_RcppExport_registerCCallable() { 
    R_RegisterCCallable("LambertW", "_LambertW_W_Cpp", (DL_FUNC)_LambertW_W_Cpp_try);
    R_RegisterCCallable("LambertW", "_LambertW_W_delta_Cpp", (DL_FUNC)_LambertW_W_delta_Cpp_try);
    R_RegisterCCallable("LambertW", "_LambertW_W_delta_alpha_Cpp", (DL_FUNC)_LambertW_W_delta_alpha_Cpp_try);
    R_RegisterCCallable("LambertW", "_LambertW_W_gamma_Cpp", (DL_FUNC)_LambertW_W_gamma_Cpp_try);
    R_RegisterCCallable("LambertW", "_LambertW_RcppExport_validate", (DL_FUNC)_LambertW_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_LambertW_W_Cpp", (DL_FUNC) &_LambertW_W_Cpp, 2},
    {"_LambertW_W_delta_Cpp", (DL_FUNC) &_LambertW_W_delta_Cpp, 2},
    {"_LambertW_W_delta_alpha_Cpp", (DL_FUNC) &_LambertW_W_delta_alpha_Cpp, 3},
    {"_LambertW_W_gamma_Cpp", (DL_FUNC) &_LambertW_W_gamma_Cpp, 3},
    {"_LambertW_kurtosis", (DL_FUNC) &_LambertW_kurtosis, 1},
    {"_LambertW_lp_norm_Cpp", (DL_FUNC) &_LambertW_lp_norm_Cpp, 2},
    {"_LambertW_lp_norm_complex_Cpp", (DL_FUNC) &_LambertW_lp_norm_complex_Cpp, 2},
    {"_LambertW_normalize_by_tau_Cpp", (DL_FUNC) &_LambertW_normalize_by_tau_Cpp, 4},
    {"_LambertW_skewness", (DL_FUNC) &_LambertW_skewness, 1},
    {"_LambertW_RcppExport_registerCCallable", (DL_FUNC) &_LambertW_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_LambertW(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
