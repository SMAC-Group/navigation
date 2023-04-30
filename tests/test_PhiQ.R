# library(navigation)
# set.seed(123)
# Fmat = matrix(rnorm(n= 51^2), nrow = 51, ncol = 51)
# Gmat = matrix(rnorm(n= 51*42), nrow = 51, ncol = 42)
# Wmat = matrix(rnorm(n= 42*42), nrow = 42, ncol = 42)
#
# # compare between pred_PhiQ_cpp and pred_PhiQ_cpp_2
# for(met_i in c("exact", as.character(seq(4)))){
#   lst_1 = pred_PhiQ_cpp(Fmat, Gmat = Gmat, Wmat = Wmat, dt = .01, method =met_i)
#   lst_2 = pred_PhiQ_cpp_2(Fmat, Gmat = Gmat, Wmat = Wmat, dt = .01, method = met_i)
#   print(all.equal(lst_1, lst_2))
# }
#
# # there is a difference in the Q component for method exact method 3 and 4
#
# # compare between  my_pred_PhiQ and pred_PhiQ_cpp_2
# my_pred_PhiQ = function(Fmat, Gmat, Wmat, dt = 0.01, order) {
#
#   # get dimensions
#   n = dim(Fmat)[1]
#
#   # define A matrix
#   A = matrix(0, nrow=2*n, ncol=2*n)
#   A[1:n, 1:n] = Fmat * dt
#   A[1:n, (n+1):(2*n)] = Gmat %*% Wmat %*% t(Gmat) * dt
#   A[(n+1):(2*n), (n+1):(2*n)] = t(Fmat) * dt
#
#   # compute B matrix
#   B = diag(2*n)
#   for (i in order:1) {
#     B = diag(2*n) + B %*% A  / i
#   }
#   Phi = B[1:n, 1:n]
#   Q = B[1:n, (n+1):(2*n)]
#
#   PhiQ=list("Phi"=Phi,"Q"=Q)
# }
#
# # test equality of Phi component with pred_PhiQ_cpp_2
# for(order_i in c(as.character(seq(15)))){
#   lst1 = pred_PhiQ_cpp_2(Fmat, Gmat = Gmat, Wmat = Wmat, dt = .01, method = order_i)
#   lst2 = my_pred_PhiQ(Fmat, Gmat = Gmat, Wmat = Wmat, dt = .01, order = order_i)
#   print(  all.equal(lst1, lst2))
# }
#
#
#
#
#
#
# # test_r = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
# # test_cpp = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "1")
# # all.equal(test_r, test_cpp)
# # test_r_2 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "2")
# # test_cpp_2 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "2")
# # all.equal(test_r_2, test_cpp_2)
# # test_r_3 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# # test_cpp_3 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# # all.equal(test_r_3, test_cpp_3)
# #
# #
# # ret = microbenchmark::microbenchmark(
# #   ex = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact"),
# #   m4 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4"),
# #   m2 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "2"),
# #   times = 1e3
# # )
#
# # Fmat = matrix(c(0.5,0,0,0.5), nrow=2)
# # Wmat = matrix(c(1), nrow=1, ncol=1)
# # Gmat = matrix(c(0,1), nrow=2)
#
#
#
#
#
#
# t1 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = 3)
# t2 = pred_PhiQ_cpp(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "3")
# all.equal(t1, t2)
#
# # test equality of pred.PhiQ and my_pred_PhiQ
# for(order in seq(4)){
#   lst1 = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = order)
#   lst2 =  my_pred_PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, order = order)
#   print(all.equal(lst1$Phi, lst2$Phi))
#   print(all.equal(lst1$Q, lst2$Q))
# }
#
#
#
#
#
#
#
#
# # test_my10 = my_pred_PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, order = 10)
# # test_re = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "exact")
# #
# # print(all.equal(test_my10$Phi, test_re$Phi))
# # print(all.equal(test_my10$Q, test_re$Q))
#
#
#
# # ret = microbenchmark::microbenchmark(
# #   iter = my_pred_PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, order = 4),
# #   orig = pred.PhiQ(Fmat = Fmat, Gmat = Gmat, Wmat = Wmat, dt = 0.01, method = "4"),
# #   times = 1e4
# # )
#
# # print(ret)
