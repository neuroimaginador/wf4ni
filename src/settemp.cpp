#include <Rcpp.h>
#include <string.h>
// #include <Rinternals.h>
#include <Rembedded.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP setTempDir(SEXP sName) {
  if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
    Rf_error("invalid path");
  R_TempDir = strdup(CHAR(STRING_ELT(sName, 0)));
  return sName;
}
