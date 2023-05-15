#' @title Plot IMU error with covariances
#' @description this function plots the estimated IMU errors with covariance of a solution computed with the \code{navigation} function
#' @param sol The set of solutions returned by the \code{navigation} function
#' @param idx Which Monte-Carlo solution to plot
#' @param error Whether to plot the error with respect to the reference or the estimated values
#' @param step Plot one time out of \code{step}
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @return A plot of the estimated IMU errors with covariance.
#' @examples
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned,
#'  system = "ned")
#' plot(traj)
#' timing <- make_timing(
#'   nav.start = 0, # time at which to begin filtering
#'   nav.end = 20,
#'   freq.imu = 100, 
#'   # frequency of the IMU, can be slower wrt trajectory frequency
#'   freq.gps = 1, 
#'   # GNSS frequency
#'   freq.baro = 1, 
#'   # barometer frequency (to disable, put it very low, e.g. 1e-5)
#'   gps.out.start = 10, # to simulate a GNSS outage, set a time before nav.end
#'   gps.out.end =15
#' )
#' # create sensor for noise data generation
#' snsr.mdl <- list()
#' # this uses a model for noise data generation
#' acc.mdl <- WN(sigma2 = 5.989778e-05) +
#'   AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) +
#'   AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) +
#'   AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
#' gyr.mdl <- WN(sigma2 = 1.503793e-06) +
#'   AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) +
#'   AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)
#' snsr.mdl$imu <- make_sensor(
#'   name = "imu",
#'   frequency = timing$freq.imu,
#'   error_model1 = acc.mdl,
#'   error_model2 = gyr.mdl
#' )
#' # RTK-like GNSS
#' gps.mdl.pos.hor <- WN(sigma2 = 0.025^2)
#' gps.mdl.pos.ver <- WN(sigma2 = 0.05^2)
#' gps.mdl.vel.hor <- WN(sigma2 = 0.01^2)
#' gps.mdl.vel.ver <- WN(sigma2 = 0.02^2)
#' snsr.mdl$gps <- make_sensor(
#'   name = "gps",
#'   frequency = timing$freq.gps,
#'   error_model1 = gps.mdl.pos.hor,
#'   error_model2 = gps.mdl.pos.ver,
#'   error_model3 = gps.mdl.vel.hor,
#'   error_model4 = gps.mdl.vel.ver
#' )
#' # Barometer
#' baro.mdl <- WN(sigma2 = 0.5^2)
#' snsr.mdl$baro <- make_sensor(
#'   name = "baro",
#'   frequency = timing$freq.baro,
#'   error_model1 = baro.mdl
#' )
#' # define sensor for Kalmna filter
#' KF.mdl <- list()
#' # make IMU sensor
#' KF.mdl$imu <- make_sensor(
#'   name = "imu",
#'   frequency = timing$freq.imu,
#'   error_model1 = acc.mdl,
#'   error_model2 = gyr.mdl
#' )
#' KF.mdl$gps <- snsr.mdl$gps
#' KF.mdl$baro <- snsr.mdl$baro
#' # perform navigation simulation
#' num.runs <- 5 # number of Monte-Carlo simulations
#' res <- navigation(
#'   traj.ref = traj,
#'   timing = timing,
#'   snsr.mdl = snsr.mdl,
#'   KF.mdl = KF.mdl,
#'   num.runs = num.runs,
#'   noProgressBar = TRUE,
#'   PhiQ_method = "2",
#'   # order of the Taylor expansion of the matrix exponential used to compute Phi and Q matrices
#'   compute_PhiQ_each_n = 10,
#'   # compute new Phi and Q matrices every n IMU steps (execution time optimization)
#'   parallel.ncores = 1,
#'   P_subsampling = timing$freq.imu
#' )
#' plot_imu_err_with_cov(res, error=FALSE)
#' 
plot_imu_err_with_cov <- function(sol, idx = 1, error = TRUE, step = 10) {
  if (dim(sol$Cov.Nav[[idx]])[1] > 9) {
    titles <- c("Acc_x", "Acc_y", "Acc_z", "Gyro_x", "Gyro_y", "Gyro_z")
    
    search_names <- c("acc_x", "acc_y", "acc_z", "gyro_x", "gyro_y", "gyro_z")
    
    t <- sol$err.imu[[idx]][1, ]
    t_p <- sol$t_p
    
    it <- seq(1, length(t), step)
    
    cols <- gg_color_hue(2, 1)
    # to define old par on exit
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    # par
    par(mfrow = c(3, 1))
    
    for (j in 0:1) {
      for (i in 1:3) {
        k <- which(startsWith(sol$state_names, search_names[i + 3 * j]))
        
        if (error) {
          v1 <- rep(0, length(it))
          v2 <- sol$err.imu[[idx]][1 + i + 3 * j, it] - colSums(sol$est.imu.states[[idx]][k - 9, it, drop = FALSE])
        } else {
          v1 <- sol$err.imu[[idx]][1 + i + 3 * j, it]
          v2 <- colSums(sol$est.imu.states[[idx]][k - 9, it, drop = FALSE])
        }
        
        dcov <- apply(sol$Cov.Nav[[idx]][, , ], 3, diag)
        dstd <- 3 * sqrt(colSums(dcov[k, , drop = FALSE]))
        dstd_int <- approx(t_p, dstd, t[it])$y
        
        v3 <- dstd_int + v1
        v4 <- -dstd_int + v1
        
        l1 <- apply(rbind(v1, v2, v3, v4), 1, lims)
        l2 <- c(min(l1[1, ]), max(l1[2, ]))
        
        plot(NA, xlab = "time [s]", ylab = titles[i + 3 * j], ylim = l2, xlim = c(t[it[1]], t[tail(it, 1)]))
        
        polygon(c(t[it], rev(t[it])), c(v3, rev(v4)), col = "#acacac60", border = NA)
        
        lines(t[it], v2, type = "l", col = cols[1])
        
        lines(t[it], v1, type = "l", col = "black", lw = 1)
      }
    }
  } else {
    warning("no error states are estimated from the filter.")
  }
}
