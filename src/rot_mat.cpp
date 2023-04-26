#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat rot_C1_cpp(double r) {
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = 1;
  out(1,1) = cos(r);
  out(1,2) = sin(r);
  out(2,1) = -sin(r);
  out(2,2) = cos(r);
  
  return out;
}

// [[Rcpp::export]]
arma::mat rot_C2_cpp(double p) {
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = cos(p);
  out(0,2) = -sin(p);
  out(1,1) = 1;
  out(2,0) = sin(p);
  out(2,2) = cos(p);
  
  return out;
}

// [[Rcpp::export]]
arma::mat rot_C3_cpp(double y) {
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = cos(y);
  out(0,1) = sin(y);
  out(1,0) = -sin(y);
  out(1,1) = cos(y);
  out(2,2) = 1;
  return out;
}

// [[Rcpp::export]]
arma::mat rot_C_i_b_cpp(double r, double p, double y){
  arma::mat m1 = rot_C1_cpp(r);
  arma::mat m2 = rot_C2_cpp(p);
  arma::mat m3 = rot_C3_cpp(y);
  arma::mat out = m1*m2*m3;
  return out;
}

// [[Rcpp::export]]
arma::mat rot_C_b_i_cpp(double r, double p, double y){
  arma::mat m = rot_C_i_b_cpp(r, p, y);
  arma::mat out = m.t();
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC1_dr_cpp(double r){
  arma::mat out(3,3, arma::fill::zeros);
  out(1,1) = -sin(r);
  out(1,2) = cos(r);
  out(2,1) = -cos(r);
  out(2,2) = -sin(r);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC2_dp_cpp(double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = -sin(p);
  out(0,2) = -cos(p);
  out(2,0) = cos(p);
  out(2,2) = -sin(p);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC3_dy_cpp(double y){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = -sin(y);
  out(0,1) = cos(y);
  out(1,0) = -cos(y);
  out(1,1) = -sin(y);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_i_b_dr_cpp(double r, double p, double y){
  arma::mat out = rot_dC1_dr_cpp(r) * rot_C2_cpp(p) * rot_C3_cpp(y);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_i_b_dp_cpp(double r, double p, double y){
  arma::mat out = rot_C1_cpp(r) * rot_dC2_dp_cpp(p) * rot_C3_cpp(y);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_i_b_dy_cpp(double r, double p, double y){
  arma::mat out = rot_C1_cpp(r) * rot_C2_cpp(p) * rot_dC3_dy_cpp(y);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_b_i_dr_cpp(double r, double p, double y){
  arma::mat out = rot_dC_i_b_dr_cpp(r, p, y).t();
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_b_i_dp_cpp(double r, double p, double y){
  arma::mat out = rot_dC_i_b_dp_cpp(r, p, y).t();
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dC_b_i_dy_cpp(double r, double p, double y){
  arma::mat out = rot_dC_i_b_dy_cpp(r, p, y).t();
  return out;
}

// [[Rcpp::export]]
arma::mat rot_Cw_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = 1;
  out(0,1) = tan(p)*sin(r);
  out(0,2) = cos(r)*tan(p);
  out(1,1) = cos(r);
  out(1,2) = -sin(r);
  out(2,1) = sin(r)/cos(p);
  out(2,2) = cos(r)/cos(p);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dCw_dr_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,1) =  cos(r)*tan(p);
  out(0,2) = -tan(p)*sin(r);
  out(1,1) = -sin(r);
  out(1,2) = -cos(r);
  out(2,1) = cos(r)/cos(p);
  out(2,2) = -sin(r)/cos(p);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dCw_dp_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,1) = sin(r)/pow(cos(p), 2 );
  out(0,2) = cos(r)/pow(cos(p),2 );
  out(2,1) = (sin(p)*sin(r))/pow(cos(p),2);
  out(2,2) = (cos(r)*sin(p))/pow(cos(p), 2);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_Cw_inv_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,0) = 1;
  out(0,2) = -sin(p);
  out(1,1) = cos(r);
  out(1,2) = cos(p)*sin(r);
  out(2,1) = -sin(r);
  out(2,2) = cos(p)*cos(r);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dCw_inv_dr_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(1,1) = -sin(r);
  out(1,2) = cos(p)*cos(r);
  out(2,1) = -cos(r);
  out(2,2) = -cos(p)*sin(r);
  return out;
}

// [[Rcpp::export]]
arma::mat rot_dCw_inv_dp_cpp(double r, double p){
  arma::mat out(3,3, arma::fill::zeros);
  out(0,2) = -cos(p);
  out(1,2) = -sin(p)*sin(r);
  out(2,2) = -cos(r)*sin(p);
  return out;
}
