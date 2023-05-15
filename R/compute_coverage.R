#' @title Compute Coverage
#' @description Compute Empirical Coverage
#' @param sols The set of solutions returned by the \code{navigation} function
#' @param alpha size of the confidence interval
#' @param step Step
#' @param idx Components of the states to be considered (default: position and orientation)
#' @param progressbar A \code{boolean} specifying whether or not to show a progress bar. Default is FALSE.
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @examples
#' # load data
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
#' timing <- make_timing(
#'   nav.start = 0, # time at which to begin filtering
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
#' AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) +
#'   AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) +
#'   AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
#' gyr.mdl <- WN(sigma2 = 1.503793e-06) + 
#' AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) +
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
#'   PhiQ_method = "1",
#'   # order of the Taylor expansion of the matrix exponential
#'   # used to compute Phi and Q matrices
#'   compute_PhiQ_each_n = 20,
#'   # compute new Phi and Q matrices
#'   # every n IMU steps (execution time optimization)
#'   parallel.ncores = 1,
#'   P_subsampling = timing$freq.imu
#' ) # keep one covariance every second
#' coverage <- compute_coverage(res, alpha = 0.7, step = 100, idx = 1:6)
#' plot(coverage)
#' @return Return a \code{coverage.stat} object which contains the empirical coverage of the navigation solutions.
compute_coverage <- function(sols, alpha = 0.95, step = 100, idx = 1:6, progressbar = FALSE) {
  if (max(idx) > 9) {
    stop("idx must be in [1, ..., 9]")
  }

  nsols <- length(sols$traj.fused)

  e <- compute_es(sols, step, idx, progressbar)

  # bounds = qchisq(c((1-alpha)/2,1-(1-alpha)/2), df=length(idx), lower.tail=TRUE)
  bounds <- qchisq(c(0, alpha), df = length(idx))

  out_b <- (e[2:(nsols + 1), ] > bounds[2]) + (e[2:(nsols + 1), ] < bounds[1])
  in_b <- -out_b + 1

  coverage <- matrix(0, nrow = 2, ncol = dim(e)[2])
  coverage[1, ] <- e[1, ]
  if (nsols > 1) {
    coverage[2, ] <- apply(in_b, 2, sum) / nsols
  } else {
    coverage[2, ] <- in_b
  }

  class(coverage) <- c("coverage.stat", "navigation.stat")
  attributes(coverage)$meta <- list(
    "type" = "Coverage",
    "unit" = NA,
    "step" = step,
    "alpha" = alpha,
    "idx" = idx,
    "nruns" = nsols,
    "t0" = NULL,
    "tend" = NULL
  )

  return(coverage)
}
