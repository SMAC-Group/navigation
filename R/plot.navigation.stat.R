#' @title Plot multiple \code{navigation.stat} objects
#' @description plot multiple stats alltogether
#' @param ... navigation statistics, e.g., computed with \code{compute_mean_position_err}
#' @export
#' @param legend The legend
#' @param title The title
#' @param xlim xlim
#' @param ylim ylim
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @return  A plot of the position or orientation error.
#' @examples
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
#' plot(traj)
#' timing <- make_timing(
#'   nav.start = 0, # time at which to begin filtering
#'   nav.end = 15,
#'   freq.imu = 100, # frequency of the IMU, can be slower wrt trajectory frequency
#'   freq.gps = 1, # GNSS frequency
#'   freq.baro = 1, # barometer frequency (to disable, put it very low, e.g. 1e-5)
#'   gps.out.start = 8, # to simulate a GNSS outage, set a time before nav.end
#'   gps.out.end = 13
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
#' num.runs <- 1 # number of Monte-Carlo simulations
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
#' # Mean orientation error
#' pe <- compute_mean_position_err(res, step = 25)
#' plot(pe)
#' 
plot.navigation.stat <- function(..., legend = NA, title = NA, xlim = c(NA, NA), ylim = c(NA, NA)) {
  stats <- list(...)
  
  # check arguments
  for (i in seq_along(stats)) {
    if (!inherits(stats[[i]], "navigation.stat")) {
      stop(paste("argument", i, "is not a navigation.stat"))
    }
  }
  
  # compute axis limits
  
  Lx <- matrix(0, nrow = length(stats), ncol = 2)
  Ly <- matrix(0, nrow = length(stats), ncol = 2)
  for (i in seq_along(stats)) {
    Lx[i, ] <- lims(stats[[i]][1, ])
    Ly[i, ] <- lims(stats[[i]][2, ])
  }
  Lx <- ifelse(is.na(xlim), c(min(Lx[, 1]), max(Lx[, 2])), xlim)
  Ly <- ifelse(is.na(ylim), c(min(Ly[, 1]), max(Ly[, 2])), ylim)
  
  # plot
  
  plot(NA,
       xlim = Lx,
       ylim = Ly,
       xlab = "Time [s]",
       ylab = attributes(stats[[1]])$meta$unit,
       main =
         if (is.na(title)) {
           attributes(stats[[1]])$meta$type
         } else {
           title
         }
  )
  
  cols <- gg_color_hue(length(stats), 1)
  
  for (i in seq_along(stats)) {
    lines(stats[[i]][1, ], stats[[i]][2, ], type = "l", col = cols[i])
  }
  
  if (!is.na(legend[[1]])) {
    legend("top", col = cols, lt = 1, legend = legend)
  }
}
