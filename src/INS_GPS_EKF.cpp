#include <RcppArmadillo.h>
#include "rot_mat.h"
#include "KF_mat.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec cnstr_e_acc_cpp(const arma::vec & X, const arma::mat & noise_info ) {
  //  define eacc vec
  arma::vec e_acc = {0,0,0};
  //  if specified speicifc noise structure in addition to 6 WN
  if(noise_info.n_rows > 6 ){
    arma::mat no_wn_noise_info = noise_info.submat(6,0,noise_info.n_rows-1, 4);
    
    for (int ax = 0; ax < 3; ++ax){
      
      double val_to_check = ax + 1;
      arma::uvec j = find(no_wn_noise_info.col(0) != 2 && no_wn_noise_info.col(1) == 1 && no_wn_noise_info.col(2) == val_to_check);
      arma::vec to_sum = X.elem(9+j);
      e_acc[ax] = arma::accu(to_sum);
    }
  }
  return e_acc;
}

// [[Rcpp::export]]
arma::vec cnstr_e_gyr_cpp(const arma::vec & X, const arma::mat & noise_info ) {
  //  define eacc vec
  arma::vec e_acc = {0,0,0};
  //  if specified specific noise structure in addition to 6 WN
  if(noise_info.n_rows > 6 ){
    arma::mat no_wn_noise_info = noise_info.submat(6,0,noise_info.n_rows-1, 4);
    
    for (int ax = 0; ax < 3; ++ax){
      
      double val_to_check = ax + 1;
      arma::uvec j = find(no_wn_noise_info.col(0) != 2 && no_wn_noise_info.col(1) == 2 && no_wn_noise_info.col(2) == val_to_check);
      arma::vec to_sum = X.elem(9+j);
      e_acc[ax] = arma::accu(to_sum);
    }
  }
  return e_acc;
}



// [[Rcpp::export]]
List pred_PhiQ_cpp(const arma::mat & Fmat, const arma::mat & Gmat, const arma::mat & Wmat, double dt, String method = "1"){
      //  only supports exact method and taxlor approximation up to the 4 degrees
      double n = Fmat.n_rows;
      if(method == "1"){
        arma::mat mydiag = arma::mat(n,n, arma::fill::eye);
        arma::mat Phi = mydiag + Fmat * dt;
        arma::mat q = Gmat * Wmat * Gmat.t();
        arma::mat Q = q*dt;
        List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
        return PhiQ;
      }else if(method == "2"){
        arma::mat mydiag = arma::mat(n,n, arma::fill::eye);
        arma::mat Phi = mydiag + Fmat * dt + Fmat * Fmat * pow(dt, 2)/2;
        arma::mat q = Gmat * Wmat * Gmat.t();
        arma::mat Q = q * dt + (Fmat * q + q * Fmat.t()) * pow(dt, 2)/2;
        List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
        return PhiQ;
      }else if(method == "3"){
        arma::mat mydiag = arma::mat(n,n, arma::fill::eye);
        arma::mat Phi  = mydiag  + Fmat*dt + arma::powmat(Fmat, 2)*pow(dt, 2)/2 + arma::powmat(Fmat, 3)*pow(dt, 3) /6;
        arma::mat q = Gmat * Wmat * Gmat.t();
        arma::mat Q1 = q * dt + (Fmat * q + q * Fmat.t()) * pow(dt, 2)/2;
        arma::mat Q4 = .5 * powmat(q * Fmat.t(), 2);
        arma::mat Q5 = (Fmat * q * Fmat.t() + 0.5 * arma::powmat(Fmat, 2) * q + Q4) * pow(dt,3) / 3;
        arma::mat Q = Q1 + Q5;
        List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
        return PhiQ;
      }else if(method == "4"){
        arma::mat A1 = arma::mat(n,n, arma::fill::eye);
        arma::mat Phi = A1 + Fmat*dt + arma::powmat(Fmat, 2)*pow(dt, 2)/2 + arma::powmat(Fmat, 3)*pow(dt, 3)/6 + 
          arma::powmat(Fmat, 4)*pow(dt, 4)/24;
        arma::mat q = Gmat * Wmat * Gmat.t();
        arma::mat Q1 = q * dt + (Fmat * q + q * Fmat.t()) * pow(dt, 2)/2;
        arma::mat Q4 = .5 * powmat(q * Fmat.t(), 2);
        arma::mat Q5 = (Fmat * q * Fmat.t() + 0.5 * arma::powmat(Fmat, 2) * q + Q4) * pow(dt,3) / 3;
        arma::mat Q_sub = Fmat * q * Fmat.t();
        arma::mat Q_sub_6 = 0.5 * arma::powmat(Q_sub, 2) + 0.5 * arma::powmat(Fmat,2) * q * Fmat.t() + (arma::powmat(Fmat, 3) * q)/6 + arma::powmat(q * Fmat.t(), 3)/6;
        arma::mat Q = Q1 + Q5 + (Q_sub_6) * pow(dt, 4) / 4 ;
        List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
        return PhiQ;
      }else if(method == "exact"){
        const arma::mat Fmat_zeroes(Fmat.n_rows, Fmat.n_cols, arma::fill::zeros);
        const arma::mat A = arma::join_horiz(
          arma::join_vert(-Fmat * dt, Fmat_zeroes),
          arma::join_vert(Gmat * Wmat * Gmat.t() * dt, Fmat.t() * dt)) ;
        arma::mat B = arma::expmat(A);
        const arma::mat B12 = B.submat(0, n, n-1, 2*n-1);
        const arma::mat B22 = B.submat(n, n, 2*n-1,  2*n-1);
        const arma::mat Phi = B22.t();
        const arma::mat Q = Phi * B12;
        List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
        return PhiQ;
        }else{
        stop("method is a string and must be 1, 2, 4 or exact");
        }
  }




// [[Rcpp::export(.isNumber)]]
bool isNumber(const std::string& str)
{
  for (char const &c : str) {
    if (std::isdigit(c) == 0) return false;
  }
  return true;
}


// [[Rcpp::export]]
List pred_PhiQ_cpp_2(const arma::mat & Fmat, const arma::mat & Gmat, const arma::mat & Wmat, double dt, std::string method){
  //  support exact method and approximation of any order
  if(method == "exact"){
    double n = Fmat.n_rows;
    // Define matrix A
    arma::mat Fmat_zeroes(Fmat.n_rows, Fmat.n_cols, arma::fill::zeros);
    arma::mat A = arma::join_horiz(
      arma::join_vert(Fmat * dt, Fmat_zeroes),
      arma::join_vert(Gmat * Wmat * Gmat.t() * dt, Fmat.t() * dt)) ;
    arma::mat B = arma::expmat(A);
    const arma::mat B12 = B.submat(0, n, n-1, 2*n-1);
    const arma::mat B22 = B.submat(n, n, 2*n-1,  2*n-1);
    const arma::mat Phi = B22.t();
    const arma::mat Q = Phi * B12;
    List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
    return PhiQ;
    //  test that method is number
  }else if(isNumber(method)){
    // convert string to number
    int order_num = std::stoi(method);
    //  dimension
    double n = Fmat.n_rows;
    arma::mat Fmat_zeroes(Fmat.n_rows, Fmat.n_cols, arma::fill::zeros);
    arma::mat A = arma::join_horiz(
      arma::join_vert(Fmat * dt, Fmat_zeroes),
      arma::join_vert(Gmat * Wmat * Gmat.t() * dt, Fmat.t() * dt)) ;
    arma::mat B = arma::mat(2*n,2*n, arma::fill::eye);
    // compute taylor approximation up to the specified number
    for(int i = order_num; i > 0; i--){
      B =  arma::mat(2*n,2*n, arma::fill::eye) + ( B * A )/ i;
    }
    int x1 = n-1;
    int x2 = 2*n-1;
    arma::mat Phi = B.submat(0,0,x1,x1);
    arma::mat Q = B.submat(0, n, x1,x2 );
    List PhiQ = List::create(Named("Phi") = Phi, Named("Q") = Q);
    return(PhiQ);
    //  if method is not a number 
  }else{
    stop("method is a string and must be either exact or an integer specifying the order of the Taylor approximation");
    }
 
}




// [[Rcpp::export]]
arma::mat test_mat(const arma::mat & Fmat, double dt){
  double n = Fmat.n_rows;
  arma::mat A1 = arma::mat(n,n, arma::fill::eye);
  arma::mat test = arma::powmat(Fmat, 2) * pow(dt, 2) / 2;
  return(test);
}

// [[Rcpp::export]]
arma::vec pred_Xe_cpp(const arma::vec & X_k, const arma::mat & noise_info, double dt){
  double dn = noise_info.n_rows - 6;
  arma::vec dX = arma::vec(dn, arma::fill::zeros);
  
  if(dn > 0){
    for(double i = 0; i < dn; ++i){
      int i_n = i+6;
      int type = noise_info(i_n, 0);
      if(type == 0){
        //  correct way to stop with error in cpp?
        throw std::range_error("Error");
      }else if(type == 2){
        for(double j = 0; j < i; ++j){
          double j_n = j+6;
          if(noise_info(j_n, 0) == 1 && noise_info(j_n, 1) == noise_info(i_n, 1) && noise_info(j_n, 2) == noise_info(i_n, 2)){
            dX[j] = dX[i];
            break;
          }
        }
      }  else if( type == 3){
        dX[i] = ( exp(-noise_info(i_n, 3) * dt) - 1) / dt * X_k[i+9];
      }
    }
    
    return(X_k.subvec(9, 8 + dn) + dX * dt);
  } 
  
  return (arma::vec(0, arma::fill::zeros));
}





// [[Rcpp::export]]
arma::vec pred_Xn_cpp(const arma::vec & X_k, const arma::vec & imu_data, const arma::mat & noise_info, double dt, double g){
  arma::vec X_ned = X_k.subvec(0,2);
  arma::vec V_ned = X_k.subvec(3,5);
  double r     = X_k[6];
  double p     = X_k[7];
  double y     = X_k[8];
  
  arma::vec f_b_raw = imu_data.subvec(1,3);
  arma::vec e_acc = cnstr_e_acc_cpp(X_k, noise_info);
  arma::vec f_b = f_b_raw - e_acc;
  
  arma::vec omega_ib_b_raw = imu_data.subvec(4,6);
  arma::vec e_gyr = cnstr_e_gyr_cpp(X_k, noise_info);
  arma::vec omega_ib_b = omega_ib_b_raw - e_gyr;
  
  arma::vec g_i = arma::vec(3, arma::fill::zeros);
  g_i[2] = g;
  
  arma::vec dX_ned = V_ned;
  arma::vec dV_ned = rot_C_b_i_cpp(r,p,y) * f_b + g_i;
  arma::vec drpy = rot_Cw_cpp(r,p) * omega_ib_b;
  
  arma::vec dX_temp = arma::join_cols(dX_ned, dV_ned);
  arma::vec dX = arma::join_cols(dX_temp, drpy);
  
  arma::vec Xn_kp1 = X_k.subvec(0,8) + dX*dt;
  
  return Xn_kp1;

}

// [[Rcpp::export]]
List EKF_pred_cpp(const arma::vec & X_k,  const arma::mat & P_k, const arma::mat & Fee,
                  const arma::mat & Wmat,  const arma::vec imu_data,  const arma::mat & noise_info, double dt, double g, String method){
  arma::vec Xn = pred_Xn_cpp(X_k, imu_data, noise_info, dt, g);
  arma::vec Xe = pred_Xe_cpp(X_k, noise_info, dt);
  arma::vec X_kp = arma::join_cols(Xn, Xe);
  
  arma::mat Fmat = KF_mat_Fmat_cpp(X_k, imu_data, noise_info, Fee);
  arma::mat Gmat = KF_mat_Gmat_cpp(X_k, noise_info);
  
  List PhiQ = pred_PhiQ_cpp_2(Fmat,Gmat,Wmat,dt,method);
    
    
  arma::mat Phi = PhiQ["Phi"];
  arma::mat Q = PhiQ["Q"];
  arma::mat P_kp = Phi * P_k * Phi.t() + Q;
  List out = List::create(Named("X_kp") = X_kp, Named("P_kp") = P_kp, Named("Phi") = Phi, Named("Q") = Q);

  return out;
}





// [[Rcpp::export]]
List EKF_pred_cpp_PhiQ_provided(const arma::vec & X_k,  const arma::mat & P_k, const arma::mat & Fee,
                  const arma::mat & Wmat,  const arma::vec imu_data,  const arma::mat & noise_info, double dt, double g, 
                  const arma::mat & last_Phi, const arma::mat & last_Q){
  arma::vec Xn = pred_Xn_cpp(X_k, imu_data, noise_info, dt, g);
  arma::vec Xe = pred_Xe_cpp(X_k, noise_info, dt);
  arma::vec X_kp = arma::join_cols(Xn, Xe);
  
  arma::mat Fmat = KF_mat_Fmat_cpp(X_k, imu_data, noise_info, Fee);
  arma::mat Gmat = KF_mat_Gmat_cpp(X_k, noise_info);
  
  arma::mat Phi = last_Phi;
  arma::mat Q = last_Q;
  arma::mat P_kp = Phi * P_k * Phi.t() + Q;
  List out = List::create(Named("X_kp") = X_kp, Named("P_kp") = P_kp, Named("Phi") = Phi, Named("Q") = Q);
  return out;
}
  
