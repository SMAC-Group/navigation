#' @title Plot multiple \code{coverage.stat} objects
#' @description plot multiple coverages alltogether
#' @param ... coverage, e.g., computed with \code{compute_coverage}
#' @param legend Legend of the plot.
#' @param title Title of the plot.
#' @return a plot of the empirical coverage.
#' @examples
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
#' plot(traj)
#' timing <- make_timing(
#'   nav.start = 0, # time at which to begin filtering
#'   nav.end = 30,
#'   freq.imu = 100, # frequency of the IMU, can be slower wrt trajectory frequency
#'   freq.gps = 1, # GNSS frequency
#'   freq.baro = 1, # barometer frequency (to disable, put it very low, e.g. 1e-5)
#'   gps.out.start = 20, # to simulate a GNSS outage, set a time before nav.end
#'   gps.out.end = 25
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
#' # Empirical coverage
#' coverage <- compute_coverage(res, alpha = 0.7, step = 100, idx = 1:6)
#' plot(coverage)
#' 
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
plot.coverage.stat <- function(..., legend = NA, title = NA) {
  stats <- list(...)
  
  # check arguments
  for (i in seq_along(stats)) {
    if (!inherits(stats[[i]], "coverage.stat")) {
      stop(paste("argument", i, "is not a coverage.stat"))
    }
  }
  
  do.call(plot.navigation.stat, c(stats, list("legend" = legend, "title" = title, "ylim" = c(0, 1))))
  
  # plot bounds
  
  alpha <- attributes(stats[[1]])$meta$alpha
  nruns <- attributes(stats[[1]])$meta$nruns
  
  bounds <- alpha + c(-1, 1) * qnorm(1 - (1 - alpha) / 2) * sqrt(alpha * (1 - alpha) / nruns)
  
  abline(h = bounds, lt = 2, col = "black")
}
