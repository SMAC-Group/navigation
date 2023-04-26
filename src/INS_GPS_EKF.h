#ifndef INS_GPS_EKF_H
#define INS_GPS_EKF_H
using namespace Rcpp;

arma::vec cnstr_e_acc_cpp(const arma::vec & X, const arma::mat & noise_info ) ;
arma::vec cnstr_e_gyr_cpp(const arma::vec & X, const arma::mat & noise_info );

#endif
