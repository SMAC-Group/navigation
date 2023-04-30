#' @importFrom stats runif
#' @importFrom rbenchmark benchmark
do_tests <- function() {
  roll <- runif(1, -pi, pi)

  R1 <- rot.C1(roll)
  R2 <- rot_C1_cpp(roll)

  print(R1 - R2)

  rbenchmark::benchmark(
    "R" = {
      rot.C1(roll)
    },
    "cpp" = {
      rot_C1_cpp(roll)
    },
    replications = 1e5
  )
}



# #########################################################################################################################
# ######################################## testing function in INS_GPS_EKF.cpp ###########################################
# #########################################################################################################################
# all.equal(cnstr_e_acc_cpp(X, noise_info) , as.matrix( cnstr.e_acc(X, noise_info)))
# all.equal(cnstr_e_gyr_cpp(X, noise_info) , as.matrix( cnstr.e_gyr(X, noise_info)))
# #########################################################################################################################
# ######################################## testing function in KF_mat.cpp ###########################################
# #########################################################################################################################
# all.equal(KF_mat_Fnn_cpp(X, imu_data, noise_info), KF_mat.Fnn(X, imu_data, noise_info))
# all.equal(KF_mat_Fee_cpp(noise_info), KF_mat.Fee(noise_info))
# all.equal(KF_mat_Fne_cpp(X, noise_info), KF_mat.Fne(X, noise_info))
# all.equal(KF_mat_Fen_cpp(noise_info), KF_mat.Fen(noise_info))
# all.equal(KF_mat_Fmat_cpp(X, imu_data, noise_info, Fee), KF_mat.Fmat(X, imu_data, noise_info, Fee))
# all.equal(KF_mat_Gmat_cpp(X, noise_info), KF_mat.Gmat(X, noise_info))
# all.equal(KF_mat_Wmat_cpp(noise_info), KF_mat.Wmat(noise_info))
#
# # test pred phiq
# Fmat = KF_mat_Fmat_cpp(X, imu_data, noise_info, Fee)
# Gmat = KF_mat_Gmat_cpp(X, noise_info)
# Wmat = KF_mat_Wmat_cpp(noise_info)
# dt = .01
# # test for method 1
# all.equal(pred_PhiQ_cpp(Fmat, Gmat, Wmat, dt, method= "1") , pred.PhiQ(Fmat, Gmat, Wmat, dt, method= "1"))
#
# # test method 2
# all.equal(pred_PhiQ_cpp(Fmat, Gmat, Wmat, dt, method= "2") , pred.PhiQ(Fmat, Gmat, Wmat, dt, method= "2"))
#
# #
# rm(list=ls())
# library(navigation)
# load("~/github_repo/navigation/data/noise_info.RData")
# set.seed(123456)
# X = rnorm(dim(noise_info)[1]-6+9)
# X_k = X
# dim_pk = dim(noise_info)[1]-6+9
# P_k = matrix(rnorm( dim_pk^2), ncol = dim_pk)
# dim_noise_info = dim(noise_info)[1]-6
# Fee = matrix(rnorm(dim_noise_info^2 ),
#              ncol = dim_noise_info,
#              nrow = dim_noise_info)
# imu_data = rnorm(7)
# dt = .01
# g = 9.8056
# Gmat = KF_mat_Gmat_cpp(X, noise_info)
# Wmat = KF_mat_Wmat_cpp(noise_info)
# Fmat = KF_mat_Fmat_cpp(X, imu_data, noise_info, Fee)
# all.equal(pred_PhiQ_cpp(Fmat, Gmat, Wmat, dt = 0.01, method= "2") , pred.PhiQ(Fmat, Gmat, Wmat, dt = 0.01, method= "2"))
# all.equal(pred_Xe_cpp(X_k = X, noise_info = noise_info, dt = .01), as.matrix(pred.Xe(X, noise_info = noise_info, dt = .01))  )
# all.equal(pred_Xn_cpp(X_k = X_k, imu_data = imu_data, noise_info = noise_info, dt = .01, g = 9.8056),
#          as.matrix( pred.Xn(X_k = X_k, imu_data = imu_data, noise_info = noise_info, dt = .01, g = 9.8056)) )
#
#
# # check for first component
# all.equal(EKF_pred_cpp(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                        noise_info = noise_info, dt = dt, g = g, method = "2")$X_kp,
#          as.matrix( EKF.pred(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                              noise_info = noise_info, dt = dt, g = g, method = "2")$X_kp)
#           )
#
# # check second argument
# all.equal(EKF_pred_cpp(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                        noise_info = noise_info, dt = dt, g = g, method = "2")$P_kp,
#           as.matrix( EKF.pred(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                               noise_info = noise_info, dt = dt, g = g, method = "2")$P_kp)
# )

# rbenchmark::benchmark("cpp" = {
#   EKF_pred_cpp(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                                        noise_info = noise_info, dt = dt, g = g, method = "2")
# },
# "R" = {
#   EKF.pred(X_k = X_k, P_k = P_k, Fee = Fee, Wmat = Wmat, imu_data = imu_data,
#                                          noise_info = noise_info, dt = dt, g = g, method = "2")
# },
# replications = 1e5
# )

# pred phiq
# library(Rcpp)
# library(RcppArmadillo)
# # library(navigation)


##############
# test method 4
##############


# rm(list=ls())
# library(navigation)
# library(expm)
# set.seed(12345)
# Fmat = matrix(rnorm(n= 51^2), nrow = 51, ncol = 51)
# Gmat = matrix(rnorm(n= 51*42), nrow = 51, ncol = 42)
# Wmat = matrix(rnorm(n= 42*42), nrow = 42, ncol = 42)
# dt = .01
# n = dim(Fmat)[1]
# test_r_4 =pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4")
# test_cpp_4 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4")
# all.equal(test_r_4$Phi, test_cpp_4$Phi)
# all.equal(test_r_4$Q, test_cpp_4$Q)
# microbenchmark::microbenchmark(pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4"),
#                                pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4")
#                                )



# test_r_3 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# test_cpp_3 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# all.equal(test_r_3, test_cpp_3)
# microbenchmark::microbenchmark(test_r_3 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact"),
#                                test_cpp_3 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# )


# test_r = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
# test_cpp = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
# all.equal(test_r, test_cpp)
# test_r_2 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "2")
# test_cpp_2 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "2")
# all.equal(test_r_2, test_cpp_2)

#
# microbenchmark::microbenchmark(test_r_3 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact"),
#                                test_cpp_3 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# )

# pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
#
# test_cpp = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
# all.equal(test_cpp, test_r)
#
# pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat,Wmat = Wmat, dt = 0.01, method = "1")


#
# library(navigation)
# pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat,Wmat = Wmat, dt = 0.01, method = "1")
