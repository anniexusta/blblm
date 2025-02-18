#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::export]]
// [[Rcpp::depends(RcppArmadillo)]]

List fastLm_impl(const arma::mat& X, const arma::colvec& y) {
  int n = X.n_rows, k = X.n_cols;

  arma::colvec coef = arma::solve(X, y);
  arma::colvec res  = y - X*coef;

  // std.errors of coefficients
  double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/(n - k);

  arma::colvec std_err = arma::sqrt(s2 * arma::diagvec(arma::pinv(arma::trans(X)*X)));

  return List::create(Named("coefficients") = coef,
                      Named("stderr")       = std_err,
                      Named("df.residual")  = n - k);
}