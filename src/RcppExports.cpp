// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// symDiff
IntegerVector symDiff(IntegerVector v1, IntegerVector v2);
RcppExport SEXP VineCopulaDAGCpp_symDiff(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(symDiff(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// setDiff
IntegerVector setDiff(IntegerVector v1, IntegerVector v2);
RcppExport SEXP VineCopulaDAGCpp_setDiff(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(setDiff(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// processNodes
List processNodes(int j, List nodes);
RcppExport SEXP VineCopulaDAGCpp_processNodes(SEXP jSEXP, SEXP nodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< List >::type nodes(nodesSEXP);
    rcpp_result_gen = Rcpp::wrap(processNodes(j, nodes));
    return rcpp_result_gen;
END_RCPP
}
// finalizeMatrix
IntegerVector finalizeMatrix(IntegerMatrix x, int i);
RcppExport SEXP VineCopulaDAGCpp_finalizeMatrix(SEXP xSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(finalizeMatrix(x, i));
    return rcpp_result_gen;
END_RCPP
}
// finalizeMatrixOuter
IntegerMatrix finalizeMatrixOuter(IntegerMatrix x, int i);
RcppExport SEXP VineCopulaDAGCpp_finalizeMatrixOuter(SEXP xSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(finalizeMatrixOuter(x, i));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"VineCopulaDAGCpp_symDiff", (DL_FUNC) &VineCopulaDAGCpp_symDiff, 2},
    {"VineCopulaDAGCpp_setDiff", (DL_FUNC) &VineCopulaDAGCpp_setDiff, 2},
    {"VineCopulaDAGCpp_processNodes", (DL_FUNC) &VineCopulaDAGCpp_processNodes, 2},
    {"VineCopulaDAGCpp_finalizeMatrix", (DL_FUNC) &VineCopulaDAGCpp_finalizeMatrix, 2},
    {"VineCopulaDAGCpp_finalizeMatrixOuter", (DL_FUNC) &VineCopulaDAGCpp_finalizeMatrixOuter, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_VineCopulaDAGCpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}