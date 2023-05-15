#' @title Compare multiple \code{navigation} objects
#' @description Comparison of error analysis plot for different trajectories
#' @param ... \code{navigation} objects to be compared.
#' @param nsim An integer indicating the number of trajectories simulated in order to compute the CI
#' @param emu_for_covmat The emulated trajectory for which to use the var-cov matrix in order to simulate data and compute the CI of the error
#' @param col_50 The color for the 50\% confidence intervals.
#' @param col_50_brd The color for the 50\% confidence intervals borders.
#' @param col_95 The color for the 95\% confidence intervals.
#' @param col_95_brd The color for the 95\% confidence intervals.
#' @param seed A seed for ploting.
#' @param col_traj_error The color of the \code{L2} norm of errors of the emulated trajectories.
#' @param time_interval_simu A value in second indicating the interval at which are simulated trajectories in order to compute the CI
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' 
#' @examples
#' \dontrun{
#  Load trajectory
#' data(example_1_traj_ned)
#' traj <- make_trajectory(data = example_1_traj_ned, 
#' system = "ned")
#' # Monte-Carlo settings----------------------------
#' num.runs <- 20
#' # Timing and sampling frequencies-----------------
#' timing <- make_timing(
#'   nav.start = 0,
#'   nav.end = 50,
#'   freq.imu = 10,
#'   freq.gps = 1,
#'   freq.baro = .5,
#'   gps.out.start = 25,
#'   gps.out.end = 45
#' )
#' # sensor model for data generation----------------
#' snsr.mdl <- list()
#' acc.mdl <- WN(sigma2 = 5.989778e-05) + 
#' AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) + 
#' AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) + 
#' AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
#' gyr.mdl <- WN(sigma2 = 1.503793e-06) + 
#' AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) + 
#' AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)
#' snsr.mdl$imu <- make_sensor(name = "imu", 
#' frequency = timing$freq.imu, error_model1 = acc.mdl, 
#' error_model2 = gyr.mdl)
#' # stochastic model for gps
#' gps.mdl.pos.hor <- WN(sigma2 = 0.025^2)
#' gps.mdl.pos.ver <- WN(sigma2 = 0.05^2)
#' gps.mdl.vel.hor <- WN(sigma2 = 0.01^2)
#' gps.mdl.vel.ver <- WN(sigma2 = 0.02^2)
#' snsr.mdl$gps <- make_sensor(
#'   name = "gps", frequency = timing$freq.gps,
#'   error_model1 = gps.mdl.pos.hor,
#'   error_model2 = gps.mdl.pos.ver,
#'   error_model3 = gps.mdl.vel.hor,
#'   error_model4 = gps.mdl.vel.ver
#' )
#' baro.mdl <- WN(sigma2 = 0.5^2)
#' snsr.mdl$baro <- make_sensor(name = "baro", 
#' frequency = timing$freq.baro, 
#' error_model1 = baro.mdl)
#' # sensor model for the KF, ideal setup ------------------------
#' KF.mdl <- list()
#' KF.mdl$imu <- make_sensor(name = "imu", 
#' frequency = timing$freq.imu, 
#' error_model1 = acc.mdl, 
#' error_model2 = gyr.mdl)
#' KF.mdl$gps <- snsr.mdl$gps
#' KF.mdl$baro <- snsr.mdl$baro
#' # sensor model for the KF, wrong model ------------------------
#' wrong_acc.mdl <- WN(sigma2 = 5.989778e-05)
#' wrong_gyr.mdl <- WN(sigma2 = 1.503793e-06)
#' wrong_KF.mdl <- list()
#' wrong_KF.mdl$imu <- make_sensor(name = "imu",
#'  frequency = timing$freq.imu, 
#'  error_model1 = wrong_acc.mdl, 
#'  error_model2 = wrong_gyr.mdl)
#' wrong_KF.mdl$gps <- snsr.mdl$gps
#' wrong_KF.mdl$baro <- snsr.mdl$baro
#' 
#' x1 <- navigation(
#'   traj.ref = traj,
#'   parallel.ncores = 1,
#'   timing = timing,
#'   snsr.mdl = snsr.mdl,
#'   KF.mdl = KF.mdl,
#'   num.runs = num.runs,
#'   noProgressBar = TRUE,
#' )
#' 
#' x2 <- navigation(
#'   traj.ref = traj,
#'   parallel.ncores = 1,
#'   timing = timing,
#'   snsr.mdl = snsr.mdl,
#'   KF.mdl = wrong_KF.mdl,
#'   num.runs = num.runs,
#'   noProgressBar = TRUE,
#' )
#' 
#' compare.navigation(x1, x2)
#' }
#' @noRd
#' @importFrom stats quantile
#' @importFrom graphics polygon
compare.navigation <- function(..., nsim = 100,
                               emu_for_covmat = 1,
                               col_50 = "#E74C3C4D", 
                               col_95 = "#F5B0414D",
                               col_50_brd = "#E74C3C",
                               col_95_brd = "#F5B041",
                               col_traj_error = "#1C12F54D", 
                               time_interval_simu = 1,
                               seed = 123) {
  # store all elements in a list
  dat <- list(...)
  ndat <- length(dat)
  names_dat <- names(dat)

  
  # to restore old par on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  # segment plotting space
  par(mfrow = c(1, ndat))

  # create lists that will countains values for all navigation object
  quantile_95_list <- list()
  quantile_50_list <- list()
  trajectory_error_list <- list() # will be a list of lists

  # iterate over all navigation objects
  for (i in 1:ndat) {
    x <- dat[[i]]

    # extract true traj
    traj <- x$traj.ref$trajectory

    # identify true trajectory
    true_traj <- cbind(traj$time, traj$x_N, traj$x_E, -traj$x_D)

    # create list that will holds values of squared error for all emulated trajectory
    squared_error_list <- list()

    # set m as the number of emulated trajectories
    m <- length(x$traj.fused)

    # for all emulated trajectories calculate the squared error
    for (j in seq(m)) {
      # extract emulated traj
      emu_traj <- x$traj.fused[[j]]$trajectory
      emu_traj <- cbind(emu_traj$time, emu_traj$x_N, emu_traj$x_E, -emu_traj$x_D)

      # create empty vector to store the squared difference
      diff_position_sum_squared <- c()

      # go over all timestep t
      for (k in seq(dim(true_traj)[1])) {
        diff <- true_traj[k, 2:4] - emu_traj[k, 2:4]

        # add to existing vector
        diff_norm <- sqrt(t(diff) %*% diff)
        diff_position_sum_squared <- c(diff_position_sum_squared, diff_norm)
      }

      # store in list the vector of squared error corresponding to that emulated path
      squared_error_list[[j]] <- diff_position_sum_squared
    }

    # create empty vectors
    quantile_95 <- c()
    quantile_50 <- c()

    # define time interval
    t_diff <- x$t[2] - x$t[1]
    obs_freq <- time_interval_simu / t_diff

    # extract all points from traj and from covmat
    length_seq <- dim(true_traj)[1]
    obs_vector <- seq(from = 2, to = length_seq, by = obs_freq)

    # for all time points in obs-vector
    for (t in obs_vector) {
      set.seed(seed)
      # isolate cov mat at time t
      my_cov_mat <- x$Cov.Nav[[emu_for_covmat]][1:3, 1:3, t]
      # simulate from a multinormal
      simulated_traj <- MASS::mvrnorm(n = nsim, mu = c(0, 0, 0), Sigma = my_cov_mat)

      # create vector of diff
      squared_diff_simu <- c()
      for (m in seq(dim(simulated_traj)[1])) {
        simu_pos <- simulated_traj[m, ]
        squared_diff_simu <- c(squared_diff_simu, sqrt(t(simu_pos) %*% simu_pos))
      }
      quantile_95 <- c(quantile_95, quantile(squared_diff_simu, probs = .95))
      quantile_50 <- c(quantile_50, quantile(squared_diff_simu, probs = .50))
    }

    # assign created elements to lists created before this loop over navigation object
    quantile_50_list[[i]] <- quantile_50
    quantile_95_list[[i]] <- quantile_95
    trajectory_error_list[[i]] <- squared_error_list
  }

  # store maximum of quantile 95 for all trajectories
  max_quantile_95 <- do.call(max, quantile_95_list)

  # plot error analysis for all objecti navigation
  for (p in seq(ndat)) {
    # define elements which will be plotted
    x <- dat[[p]]
    quantile_95 <- quantile_95_list[[p]]
    quantile_50 <- quantile_50_list[[p]]

    # create empty plot
    plot(NA, ylim = c(0, max_quantile_95), xlim = range(x$traj.fused[[1]]$trajectory$time), xlab = "Time (sec)", ylab = "Position error (norm)")

    # plot 95 % confidence interval
    # define time interval
    t_diff <- x$t[2] - x$t[1]
    obs_freq <- time_interval_simu / t_diff

    # extract all points from traj and from covmat
    length_seq <- dim(true_traj)[1]
    obs_vector <- seq(from = 2, to = length_seq, by = obs_freq)

    # define my_x
    my_x <- obs_vector * t_diff

    polygon(
      x = c(my_x, rev(my_x)),
      y = c(rep(0, length(my_x)), rev(quantile_95)),
      col = col_95,
      border = col_95_brd
    )

    # plot 50% confidence intervals
    polygon(
      x = c(my_x, rev(my_x)),
      y = c(rep(0, length(my_x)), rev(quantile_50)),
      col = col_50,
      border = col_50_brd
    )

    # plot error lines
    squared_error_list <- trajectory_error_list[[p]]
    m <- length(squared_error_list)

    # redefine my x as the total number of data point on the fused traj
    my_x <- x$t
    for (k in seq(m)) {
      lines(my_x, squared_error_list[[k]], col = col_traj_error)
    }
  }
  par(mfrow = c(1, 1))
}
