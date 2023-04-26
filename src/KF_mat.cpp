#include <RcppArmadillo.h>
#include "rot_mat.h"
#include "INS_GPS_EKF.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat KF_mat_Fnn_cpp(const arma::vec & X, const arma::vec & imu_data, const arma::mat & noise_info) {
  double r = X[6];
  double p = X[7];
  double y = X[8];
  
  arma::vec f_b_raw = imu_data.subvec(1,3);
  arma::vec e_acc = cnstr_e_acc_cpp(X, noise_info);
  arma::vec f_b = f_b_raw - e_acc;
  
  arma::vec omega_ib_b_raw = imu_data.subvec(4,6);
  arma::vec e_gyr = cnstr_e_gyr_cpp(X, noise_info);
  arma::vec omega_ib_b = omega_ib_b_raw - e_gyr;
  
  arma::mat Fnn = arma::mat(9,9, arma::fill::zeros);
  Fnn(0,3) = Fnn(1,4) = Fnn(2,5) = 1;

  // //  d/ dr
  Fnn.submat(3,6,5,6) = rot_dC_b_i_dr_cpp(r,p,y) * f_b;
  Fnn.submat(3,7,5,7) = rot_dC_b_i_dp_cpp(r,p,y) * f_b;
  Fnn.submat(3,8,5,8) = rot_dC_b_i_dy_cpp(r,p,y) * f_b;

  //  d/drpy
  Fnn.submat(6,6,8,6) = rot_dCw_dr_cpp(r,p) * omega_ib_b;
  Fnn.submat(6,7,8,7) = rot_dCw_dp_cpp(r,p) * omega_ib_b;
  
  return Fnn;
}

// [[Rcpp::export]]
arma::mat KF_mat_Fee_cpp(const arma::mat & noise_info) {
  double dn = noise_info.n_rows - 6;
  
  arma::mat Fee = arma::mat(dn, dn, arma::fill::zeros);
  
  if(dn > 0){
    for(double i = 0; i < dn; ++i){
      double i_n = i+6;
      double type = noise_info(i_n, 0);
      if(type == 1){
        // nothing$
      }else if(type == 2){
        //  this needs to be further tested with a noise_info object with type 2 rows
        
        for(double j = 0; j < i; ++j){
          double j_n = j + 6;
          if(noise_info(j_n,0) == 1 && noise_info(j_n,1) == noise_info(i_n,1) && noise_info(j_n,2) == noise_info(i_n,2)){
            Fee(j,i) = 1;
            break;
          }
        }
      } else if(type == 3){
        Fee(i,i) = -noise_info(i_n, 3);
      } else{
        //  correct way to stop with error in cpp?
        throw std::range_error("Error");
      }
    }
  }
  return Fee;
}
      
// [[Rcpp::export]]
arma::mat KF_mat_Fne_cpp(const arma::vec & X, const arma::mat & noise_info) {
  
  double r = X[6];
  double p = X[7];
  double y = X[8];
  
  arma::mat C_b_i = rot_C_b_i_cpp(r, p, y);
  arma::mat Cw    = rot_Cw_cpp(r,p);
  
  double dn = noise_info.n_rows - 6;
  arma::mat Fne = arma::mat(9, dn, arma::fill::zeros);
  
  if(dn > 0){
    
    for(int i = 0; i < dn; ++i){
      
      int i_n = i+6;
      int type = noise_info(i_n, 0);
      
      
      if(type == 1 || type == 3){ 
        // RW or GM
        if(noise_info(i_n, 1) == 1){
          // accelerometer
          int col_i = noise_info(i_n, 2)-1;
          Fne.submat(3,i,5,i) = - C_b_i.col(col_i);
        } else if(noise_info(i_n, 1) == 2){
          // gyroscope
          int col_i = noise_info(i_n, 2)-1;
          Fne.submat(6,i,8,i) = -Cw.col(col_i);
        }else{
          // some mess
          throw std::range_error("Error");
        }
      } else if(type == 2){
        // DR
        // do nothing
      }else{
        throw std::range_error("Error");
      }
    }
  }
  return Fne;
  
}
    
// [[Rcpp::export]]
arma::mat KF_mat_Fen_cpp(const arma::mat & noise_info) {
  double dn = noise_info.n_rows - 6;
  arma::mat Fen = arma::mat(dn, 9, arma::fill::zeros);
  return Fen;
  
  
}

// [[Rcpp::export]]
arma::mat KF_mat_Fmat_cpp(const arma::vec X, const arma::vec imu_data, const arma::mat & noise_info, const arma::mat & Fee) {
  arma::mat Fnn = KF_mat_Fnn_cpp(X, imu_data, noise_info);
  arma::mat Fne = KF_mat_Fne_cpp(X, noise_info);
  arma::mat Fen = KF_mat_Fen_cpp(noise_info);
  arma::mat Fmat = arma::join_horiz(arma::join_vert(Fnn, Fen), arma::join_vert(Fne, Fee));
  return Fmat;
   
}

// [[Rcpp::export]]
arma::mat KF_mat_Gmat_cpp(const arma::vec X, const arma::mat & noise_info) {
  
  double r = X[6];
  double p = X[7];
  double y = X[8];
  arma::mat C_b_i = rot_C_b_i_cpp(r, p, y);
  arma::mat Cw    = rot_Cw_cpp(r,p);
  double dn = noise_info.n_rows - 6;
  //  how many driving noises
  arma::uvec j  = find(noise_info.col(0) != 2); 
  int dd = j.n_elem;
  arma::mat Gmat = arma::mat(dn+9, dd, arma::fill::zeros);
  
  for(double i = 0; i < dd ; ++i){
    double i_n = j[i];
    if(noise_info(i_n, 0) == 0){
      //  whine noise
      if(noise_info(i_n, 1) == 1){
        // accelerometer
        int col_i = noise_info(i_n,2) - 1 ;
        Gmat.submat(3,i,5,i) = C_b_i.col(col_i);
      }else{
        int col_i = noise_info(i_n,2) -1;
        Gmat.submat(6,i,8,i) = Cw.col(col_i);
      }
    } else{
      Gmat(9+i_n-6,i) = 1;
    }
  }
  return Gmat;
}

// [[Rcpp::export]]
arma::mat KF_mat_Wmat_cpp(const arma::mat & noise_info) {
  arma::uvec j = find(noise_info.col(0) != 2);
  arma::vec sigma2 = noise_info.col(4);
  arma::vec tofill = sigma2.elem(j);
  arma::mat Wmat = arma::diagmat(tofill);
  return Wmat;
}
  
