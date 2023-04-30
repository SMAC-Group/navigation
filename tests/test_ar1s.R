# library(navigation)
#
# # ----------------- Reference trajectory
# data("lemniscate_traj_ned") # trajectory in proper format as shown bellow
# head(lemniscate_traj_ned)
# traj = make_trajectory(data = lemniscate_traj_ned, system = "ned")
#
# # ----------------- Timing and sampling frequencies
#
# timing = make_timing(nav.start     = 0, # time at which to begin filtering
#                      nav.end       = 150,
#                      freq.imu      = 10, # frequency of the IMU, can be slower wrt trajectory frequency
#                      freq.gps      = 10, # gnss frequency
#                      freq.baro     = 1e-6, # barometer frequency (to disable, put it very low, e.g. 1e-5)
#                      gps.out.start = 300, # to simulate a GNSS outage, set a time before nav.end
#                      gps.out.end   = 300)
#
# # ---------------- sensor model for data generation
# res = list()
#
# n = 1
# for (phi in seq(from=0.5, to = 1-1e-6, length.out = 10)) {
#
#   snsr.mdl=list()
#
#   # GMWM fit of MTIg IMU
#
#   # this uses a model for noise data generation
#
#   acc.mdl = AR1(phi = phi, sigma2 = 1.026718e-8)
#   gyr.mdl = AR1(phi = phi, sigma2 = 1.238142e-8)
#
#   # acc.mdl = RW(gamma2 = 1.026718e-8)
#   # gyr.mdl = RW(gamma2 = 1.238142e-8)
#
#
#   snsr.mdl$imu = make_sensor(name="imu", frequency=timing$freq.imu, error_model1=acc.mdl, error_model2=gyr.mdl)
#
#   # this uses real imu data for noise data generation
#
#   # library(imudata)
#   # data("mtig100hrz")
#   #
#   # # create a simts::imu object with data from the i-th acquisition in the mtig100hrz dataset
#   # i_acq = 1
#   # mtig_imu = simts::imu(
#   #   data = cbind(
#   #     mtig100hrz[[4]][[i_acq]]$data,
#   #     mtig100hrz[[5]][[i_acq]]$data,
#   #     mtig100hrz[[6]][[i_acq]]$data,
#   #     mtig100hrz[[1]][[i_acq]]$data,
#   #     mtig100hrz[[2]][[i_acq]]$data,
#   #     mtig100hrz[[3]][[i_acq]]$data),
#   #   gyros = c(1,2,3),
#   #   accels = c(4,5,6),
#   #   axis = c('X', 'Y', 'Z', 'X', 'Y', 'Z'),
#   #   freq = 100,
#   #   name = "mti-g")
#   #
#   # snsr.mdl$imu = make_sensor(name="imu", frequency = timing$freq.imu, error_data1 = mtig_imu)
#
#   # RTK-like GNSS
#
#   gps.mdl.pos.hor = WN(sigma2 = 0.025^2)
#   gps.mdl.pos.ver = WN(sigma2 = 0.05^2)
#   gps.mdl.vel.hor = WN(sigma2 = 0.01^2)
#   gps.mdl.vel.ver = WN(sigma2 = 0.02^2)
#   snsr.mdl$gps = make_sensor(name="gps", frequency=timing$freq.gps,
#                              error_model1=gps.mdl.pos.hor,
#                              error_model2=gps.mdl.pos.ver,
#                              error_model3=gps.mdl.vel.hor,
#                              error_model4=gps.mdl.vel.ver)
#
#   # Barometer
#
#   baro.mdl = WN(sigma2=0.5^2)
#   snsr.mdl$baro = make_sensor(name="baro", frequency=timing$freq.baro, error_model1=baro.mdl)
#
#   # ---------------- sensor model for the KF
#
#   KF.mdl = list()
#
#   # here we configure the model to be used in the KF
#
#   acc.mdl = AR1(phi = phi, sigma2 = 1.026718e-8)
#   gyr.mdl = AR1(phi = phi, sigma2 = 1.238142e-8)
#
#   KF.mdl$imu = make_sensor(name="imu", frequency=timing$freq.imu, error_model1=acc.mdl, error_model2=gyr.mdl)
#
#   KF.mdl$gps  = snsr.mdl$gps
#   KF.mdl$baro = snsr.mdl$baro
#
#   # ---------------------------- Running the navigation (Monte-Carlo)
#   num.runs = 24#24*5 # number of Monte-Carlo simulations
#   res[[n]] = navigation(traj.ref = traj,
#                    timing = timing,
#                    snsr.mdl = snsr.mdl,
#                    KF.mdl = KF.mdl,
#                    num.runs = num.runs,
#                    noProgressBar = TRUE,
#
#                    PhiQ_method=2,
#                    parallel.ncores=6,
#                    P_subsampling = timing$freq.imu) # keep one covariance every second
#
#   n = n + 1
# }
#
# # plot(res, plot3d = F, time_interval = .5, emu_for_covmat = 1,
# #      emu_to_plot = 1, time_interval_simu = 1, error_analysis = F,
# #      plot_mean_traj = T, plot_CI = F, nsim = num.runs)
#
# # ---------------------------- Statistics
#
# # pe = compute_mean_position_err(res, step = 25)
#
# # oe = compute_mean_orientation_err(res, step = 25)
#
# # nees = compute_nees(res, idx=1:6, step = 100)
#
# # coverage = compute_coverage(res, alpha = 0.7, step = 100, idx = 1:6)
#
# # ---------------------------- Plotting
#
# # plot_imu_err_with_cov(res, error=F)
# # plot_nav_states_with_cov(res,error=T, idx=1:24)
#
# # plot(pe)
# # plot(oe)
#
# # plot(nees)
# # plot(coverage)
#
# # coverage_imu_acc = compute_coverage_imu_states(res, alpha = 0.7, step = 100, idx = 1:3)
# # coverage_imu_gyro = compute_coverage_imu_states(res, alpha = 0.7, step = 100, idx = 4:6)
# #
# # par(mfrow=c(1,1))
# # plot(coverage_imu_acc,coverage_imu_gyro)
# #
# # plot_imu_err_with_cov(res, idx=1, error=T, step=10)
#
# ci = rep(0, length(res))
# for (j in seq_along(res)) {
#   cc = compute_coverage_imu_states(res[[j]], alpha = 0.7, step = 100, idx = 1:6)
#   ci[j] = mean(cc[2,])
# }
#
# plot(seq(from=0.5, to = 1-1e-6, length.out = 10), ci, ylim=c(0, 1), xlab = "phi", ylab="coverage", main="PhiQ method = 2, alpha = 0.7")
#
# alpha = 0.7
# d = sqrt(alpha*(1-alpha)/num.runs)/2
#
# abline(h = alpha+d, lt=2, col="black")
# abline(h = alpha-d, lt=2, col="black")
#
