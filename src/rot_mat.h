#ifndef rot_mat_H
#define rot_mat_H

arma::mat rot_C1_cpp(double r) ;
arma::mat rot_C2_cpp(double p);
arma::mat rot_C3_cpp(double y) ;
arma::mat rot_C_i_b_cpp(double r, double p, double y);
arma::mat rot_C_b_i_cpp(double r, double p, double y);
arma::mat rot_dC1_dr_cpp(double r);
arma::mat rot_dC2_dp_cpp(double p);
arma::mat rot_dC3_dy_cpp(double y);
arma::mat rot_dC_i_b_dr_cpp(double r, double p, double y);
arma::mat rot_dC_i_b_dp_cpp(double r, double p, double y);
arma::mat rot_dC_i_b_dy_cpp(double r, double p, double y);
arma::mat rot_dC_b_i_dr_cpp(double r, double p, double y);
arma::mat rot_dC_b_i_dp_cpp(double r, double p, double y);
arma::mat rot_dC_b_i_dy_cpp(double r, double p, double y);
arma::mat rot_Cw_cpp(double r, double p);
arma::mat rot_dCw_dr_cpp(double r, double p);
arma::mat rot_dCw_dp_cpp(double r, double p);
arma::mat rot_Cw_inv_cpp(double r, double p);
arma::mat rot_dCw_inv_dr_cpp(double r, double p);
arma::mat rot_dCw_inv_dp_cpp(double r, double p);



#endif
