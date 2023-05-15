#' @title Compute mean orientation error
#' @description Compute the mean orientation error (|| log(A^T * B) ||)
#' @param sols The set of solutions returned by the \code{navigation} function
#' @param step do it for one sample out of \code{step}
#' @param t0 Start time for RMS calculation (default: beginning)
#' @param tend Start time for RMS calculation (default: end)
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @return Return a \code{navigation.stat} object which contains the mean orientation error over the fused trajectories.
#' @examples
#' # load data
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
#' timing <- make_timing(
#'   nav.start = 0,
#'   # time at which to begin filtering
#'   nav.end = 20,
#'   freq.imu = 100,
#'   # frequency of the IMU, can be slower wrt trajectory frequency
#'   freq.gps = 1, # GNSS frequency
#'   freq.baro = 1,
#'   # barometer frequency (to disable, put it very low, e.g. 1e-5)
#'   gps.out.start = 10,
#'   # to simulate a GNSS outage, set a time before nav.end
#'   gps.out.end = 15
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
#' # define sensor for Kalman filter
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
#'   PhiQ_method = "1",
#'   # order of the Taylor expansion of the matrix exponential
#'   # used to compute Phi and Q matrices
#'   compute_PhiQ_each_n = 10,
#'   # compute new Phi and Q matrices every n IMU steps (execution time optimization)
#'   parallel.ncores = 1,
#'   P_subsampling = timing$freq.imu
#' ) # keep one covariance every second
#'
#'
#' oe <- compute_mean_orientation_err(res, step = 25)
#' plot(oe)
#'
compute_mean_orientation_err <- function(sols, step = 1, t0 = NULL, tend = NULL) {
  nsols <- length(sols$traj.fused)

  it <- get_it(sols$traj.fused[[1]]$trajectory[, 1], t0, tend, step)

  e <- matrix(0, nrow = nsols, ncol = length(it))

  n <- 0
  for (i in 1:nsols) {
    for (j in 1:length(it)) {
      e[i, j] <-
        norm(rot.log(
          t(rot.C_i_b(sols$traj.ref$trajectory[it[j], 5], sols$traj.ref$trajectory[it[j], 6], sols$traj.ref$trajectory[it[j], 7])) %*%
            rot.C_i_b(sols$traj.fused[[i]]$trajectory[it[j], 5], sols$traj.fused[[i]]$trajectory[it[j], 6], sols$traj.fused[[i]]$trajectory[it[j], 7])
        ), type = "2") / pi * 180
    }
  }

  de <- matrix(0, nrow = 2, ncol = length(it))
  de[1, ] <- sols$traj.ref$trajectory[it, 1]

  if (nsols > 1) {
    de[2, ] <- apply(e[1:nsols, ], 2, mean)
  } else {
    de[2, ] <- e[i, ]
  }

  class(de) <- "navigation.stat"
  attributes(de)$meta <- list(
    "type" = "Mean orientation error",
    "unit" = "degrees",
    "step" = step,
    "t0" = t0,
    "tend" = tend
  )

  return(de)
}
