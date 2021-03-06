// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_LambertW_RCPPEXPORTS_H_GEN_
#define RCPP_LambertW_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace LambertW {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("LambertW", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("LambertW", "_LambertW_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in LambertW");
            }
        }
    }

    inline NumericVector W_Cpp(const NumericVector& z, int branch) {
        typedef SEXP(*Ptr_W_Cpp)(SEXP,SEXP);
        static Ptr_W_Cpp p_W_Cpp = NULL;
        if (p_W_Cpp == NULL) {
            validateSignature("NumericVector(*W_Cpp)(const NumericVector&,int)");
            p_W_Cpp = (Ptr_W_Cpp)R_GetCCallable("LambertW", "_LambertW_W_Cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_W_Cpp(Shield<SEXP>(Rcpp::wrap(z)), Shield<SEXP>(Rcpp::wrap(branch)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector W_delta_Cpp(const NumericVector& z, double delta) {
        typedef SEXP(*Ptr_W_delta_Cpp)(SEXP,SEXP);
        static Ptr_W_delta_Cpp p_W_delta_Cpp = NULL;
        if (p_W_delta_Cpp == NULL) {
            validateSignature("NumericVector(*W_delta_Cpp)(const NumericVector&,double)");
            p_W_delta_Cpp = (Ptr_W_delta_Cpp)R_GetCCallable("LambertW", "_LambertW_W_delta_Cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_W_delta_Cpp(Shield<SEXP>(Rcpp::wrap(z)), Shield<SEXP>(Rcpp::wrap(delta)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector W_delta_alpha_Cpp(const NumericVector& z, double delta, double alpha) {
        typedef SEXP(*Ptr_W_delta_alpha_Cpp)(SEXP,SEXP,SEXP);
        static Ptr_W_delta_alpha_Cpp p_W_delta_alpha_Cpp = NULL;
        if (p_W_delta_alpha_Cpp == NULL) {
            validateSignature("NumericVector(*W_delta_alpha_Cpp)(const NumericVector&,double,double)");
            p_W_delta_alpha_Cpp = (Ptr_W_delta_alpha_Cpp)R_GetCCallable("LambertW", "_LambertW_W_delta_alpha_Cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_W_delta_alpha_Cpp(Shield<SEXP>(Rcpp::wrap(z)), Shield<SEXP>(Rcpp::wrap(delta)), Shield<SEXP>(Rcpp::wrap(alpha)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector W_gamma_Cpp(const NumericVector& z, double gamma, int branch) {
        typedef SEXP(*Ptr_W_gamma_Cpp)(SEXP,SEXP,SEXP);
        static Ptr_W_gamma_Cpp p_W_gamma_Cpp = NULL;
        if (p_W_gamma_Cpp == NULL) {
            validateSignature("NumericVector(*W_gamma_Cpp)(const NumericVector&,double,int)");
            p_W_gamma_Cpp = (Ptr_W_gamma_Cpp)R_GetCCallable("LambertW", "_LambertW_W_gamma_Cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_W_gamma_Cpp(Shield<SEXP>(Rcpp::wrap(z)), Shield<SEXP>(Rcpp::wrap(gamma)), Shield<SEXP>(Rcpp::wrap(branch)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

}

#endif // RCPP_LambertW_RCPPEXPORTS_H_GEN_
