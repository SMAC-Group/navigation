#ifndef KF_mat_H
#define KF_mat_H

arma::mat KF_mat_Fee_cpp(const arma::mat & noise_info);
arma::mat KF_mat_Fnn_cpp(const arma::vec & X, const arma::vec & imu_data, const arma::mat & noise_info);
arma::mat KF_mat_Fne_cpp(const arma::vec & X, const arma::mat & noise_info);
arma::mat KF_mat_Fen_cpp(const arma::mat & noise_info) ;
arma::mat KF_mat_Fmat_cpp(const arma::vec X, const arma::vec imu_data, const arma::mat & noise_info, const arma::mat & Fee);
arma::mat KF_mat_Gmat_cpp(const arma::vec X, const arma::mat & noise_info) ;
arma::mat KF_mat_Wmat_cpp(const arma::mat & noise_info) ;
    


#endif
