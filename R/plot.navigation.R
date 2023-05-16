#' @title Plot a \code{navigation} object
#' @description This function enables the visualization of a \code{navigation} object, both in 2d and in 3d. The function therefore enables
#' the comparison of the true trajectory with emulated trajectories. One can also plot the analysis of the error of the trajectories
#' by comparing the \code{L2} norm of the difference between emulated trajectories and the true trajectory over time.
#' @param x A \code{navigation} object
#' @param true_col The color of the true trajectory
#' @param col_fused_trans The color of the emulated trajectories
#' @param col_fused_full The color of the mean trajectory of the emulated trajectories
#' @param plot_mean_traj A Boolean indicating whether or not to plot the mean mean trajectory of the emulated trajectories. Default is \code{True}
#' @param plot_baro A Boolean indicating whether or not to plot the barometer datapoint in tha Up coordinates plot. Default is \code{True}
#' @param baro_col The color of the barometer datapoints
#' @param emu_to_plot The emulated trajectory for which to plot confidence ellipses on the North-East coordinates plot
#' @param plot3d A Boolean indicating whether or not to plot the 3d plot of the trajectory
#' @param plot_CI A Boolean indicating whether or not to plot the confidence intervals for both 2d plots
#' @param time_interval A value in seconds indicating the interval at which to plot the CI on the North-East coordinates plot
#' @param col_50 The color for the 50\% confidence intervals.
#' @param col_50_brd The color for the 50\% confidence intervals borders.
#' @param col_95 The color for the 95\% confidence intervals.
#' @param col_95_brd The color for the 95\% confidence intervals.
#' @param error_analysis A Boolean indicating whether or not to display an error analysis plot of the emulated trajectories
#' @param emu_for_covmat The emulated trajectory for which to use the var-cov matrix in order to simulate data and compute the CI of the error
#' @param nsim An integer indicating the number of trajectories simulated in order to compute the CI
#' @param col_traj_error The color for the trajectory estimation error
#' @param ... additional plotting argument
#' @param time_interval_simu time interval simu
#' @param seed A seed for plotting
#' @return A 2D or 3D plot of the trajectory with the fused trajectories.
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom MASS mvrnorm
#' @importFrom magrittr %>%
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @examples
#' data("lemniscate_traj_ned")
#' head(lemniscate_traj_ned)
#' traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
#' plot(traj)
#' timing <- make_timing(
#'   nav.start = 0, # time at which to begin filtering
#'   nav.end = 20,
#'   freq.imu = 100, # frequency of the IMU, can be slower wrt trajectory frequency
#'   freq.gps = 1, # GNSS frequency
#'   freq.baro = 1, # barometer frequency (to disable, put it very low, e.g. 1e-5)
#'   gps.out.start = 5, # to simulate a GNSS outage, set a time before nav.end
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
#'   PhiQ_method = "3",
#'   # order of the Taylor expansion of the matrix exponential used to compute Phi and Q matrices
#'   compute_PhiQ_each_n = 10,
#'   # compute new Phi and Q matrices every n IMU steps (execution time optimization)
#'   parallel.ncores = 1,
#'   P_subsampling = timing$freq.imu
#' )
#' plot(res)
#' # 3D plot
#' plot(res, plot3d = TRUE)
#' plot(res, error_analysis = TRUE)
#' @export
#' @importFrom stats qnorm
#' @importFrom graphics points layout polygon
#' @importFrom plotly add_markers
plot.navigation <- function(x,
                            true_col = "#2980b9",
                            col_fused_trans = "#EA5D0073",
                            col_fused_full = "#EA5D00FF",
                            plot_mean_traj = TRUE,
                            plot_baro = TRUE,
                            baro_col = "black",
                            emu_to_plot = 1,
                            plot3d = FALSE,
                            plot_CI = FALSE,
                            time_interval = 5,
                            col_50 = "#E74C3C4D",
                            col_95 = "#F5B0414D",
                            col_50_brd = "#E74C3C",
                            col_95_brd = "#F5B041",
                            error_analysis = FALSE,
                            emu_for_covmat = 1,
                            nsim = 1000,
                            col_traj_error = "#1C12F54D",
                            time_interval_simu = .5,
                            seed = 123,
                            ...) {
  # -------------------- debug
  # data("lemniscate_traj_ned")
  # head(lemniscate_traj_ned)
  # traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
  # plot(traj)
  # timing <- make_timing(
  #   nav.start = 0, # time at which to begin filtering
  #   nav.end = 100,
  #   freq.imu = 100, # frequency of the IMU, can be slower wrt trajectory frequency
  #   freq.gps = 1, # GNSS frequency
  #   freq.baro = 1, # barometer frequency (to disable, put it very low, e.g. 1e-5)
  #   gps.out.start = 60, # to simulate a GNSS outage, set a time before nav.end
  #   gps.out.end = 90
  # )
  # # create sensor for noise data generation
  # snsr.mdl <- list()
  # # this uses a model for noise data generation
  # acc.mdl <- WN(sigma2 = 5.989778e-05) + AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) +
  #   AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) + AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
  # gyr.mdl <- WN(sigma2 = 1.503793e-06) + AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) +
  #   AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)
  # snsr.mdl$imu <- make_sensor(
  #   name = "imu",
  #   frequency = timing$freq.imu,
  #   error_model1 = acc.mdl,
  #   error_model2 = gyr.mdl
  # )
  # # RTK-like GNSS
  # gps.mdl.pos.hor <- WN(sigma2 = 0.025^2)
  # gps.mdl.pos.ver <- WN(sigma2 = 0.05^2)
  # gps.mdl.vel.hor <- WN(sigma2 = 0.01^2)
  # gps.mdl.vel.ver <- WN(sigma2 = 0.02^2)
  # snsr.mdl$gps <- make_sensor(
  #   name = "gps",
  #   frequency = timing$freq.gps,
  #   error_model1 = gps.mdl.pos.hor,
  #   error_model2 = gps.mdl.pos.ver,
  #   error_model3 = gps.mdl.vel.hor,
  #   error_model4 = gps.mdl.vel.ver
  # )
  # # Barometer
  # baro.mdl <- WN(sigma2 = 0.5^2)
  # snsr.mdl$baro <- make_sensor(name = "baro",
  #                              frequency = timing$freq.baro,
  #                              error_model1 = baro.mdl)
  # # define sensor for Kalmna filter
  # KF.mdl <- list()
  # # make IMU sensor
  # KF.mdl$imu <- make_sensor(
  #   name = "imu",
  #   frequency = timing$freq.imu,
  #   error_model1 = acc.mdl,
  #   error_model2 = gyr.mdl
  # )
  # KF.mdl$gps <- snsr.mdl$gps
  # KF.mdl$baro <- snsr.mdl$baro
  # # perform navigation simulation
  # num.runs <- 5 # number of Monte-Carlo simulations
  # res <- navigation(
  #   traj.ref = traj,
  #   timing = timing,
  #   snsr.mdl = snsr.mdl,
  #   KF.mdl = KF.mdl,
  #   num.runs = num.runs,
  #   noProgressBar = FALSE,
  #   PhiQ_method = "4",
  #   # order of the Taylor expansion of the matrix exponential used to compute Phi and Q matrices
  #   compute_PhiQ_each_n = 10,
  #   # compute new Phi and Q matrices every n IMU steps (execution time optimization)
  #   parallel.ncores = 8,
  #   P_subsampling = timing$freq.imu
  # )
  # plot(res)
  # plot(res, plot3d = T)
  # plot(res, error_analysis = TRUE)


  #
  #
  #
  # --------------------------------------------------------- define var
  # x = res
  # emu_to_plot = 1
  # time_interval = 5
  # plot_CI = F
  # true_col = "#2980b9"
  # col_fused_trans = "#EA5D0073"
  # plot_mean_traj = TRUE
  # col_fused_full = "#EA5D00FF"
  # plot_baro =TRUE
  # plot_baro = TRUE
  # baro_col = "black"
  # error_analysis =TRUE
  # nsim=5
  # col_50_brd = "#E74C3C"
  # col_95_brd = "#F5B041"
  # error_analysis = TRUE
  # col_50 = "#E74C3C4D"
  # col_95 = "#F5B0414D"
  # col_traj_error = "#1C12F54D"
  # plot3d = T
  #

  # -------------------------------------------------------- debug




  # extract elements from navigation object
  traj <- x$traj.ref$trajectory
  n <- nrow(traj)

  # Number of trajectories
  m <- length(x$traj.fused)

  if (emu_to_plot > m) stop("You can not enter a emulation path to plot larger than the number of emulation paths in the navigation object")

  # Compute range
  max_ref_x <- max(traj$x_E)
  min_ref_x <- min(traj$x_E)
  max_ref_y <- max(traj$x_N)
  min_ref_y <- min(traj$x_N)
  max_ref_z <- max(-traj$x_D)
  min_ref_z <- min(-traj$x_D)

  # create empty vector
  max_fused_x <- min_fused_x <- rep(NA, m)
  max_fused_y <- min_fused_y <- rep(NA, m)
  max_fused_z <- min_fused_z <- rep(NA, m)

  # define vector of maximum and minimum for all three axis x y z for all fused trajectories
  for (i in 1:m) {
    xE <- x$traj.fused[[i]]$trajectory$x_E
    xN <- x$traj.fused[[i]]$trajectory$x_N
    xU <- -x$traj.fused[[i]]$trajectory$x_D

    max_fused_x[i] <- max(xE)
    min_fused_x[i] <- min(xE)
    max_fused_y[i] <- max(xN)
    min_fused_y[i] <- min(xN)
    max_fused_z[i] <- max(xU)
    min_fused_z[i] <- min(xU)
  }

  # establish the range for all three axis based
  # on the min/max of all fused trajectories and the true traj
  range_x <- c(min(c(min_fused_x, min_ref_x)), max(c(max_fused_x, max_ref_x)))
  range_y <- c(min(c(min_fused_y, min_ref_y)), max(c(max_fused_y, max_ref_y)))
  range_z <- c(min(c(min_fused_z, min_ref_z)), max(c(max_fused_z, max_ref_z)))

  # calculate center of it
  center_x <- mean(range_x)
  center_y <- mean(range_y)

  # make the diff of both ranges x and y equal
  if (diff(range_x) >= diff(range_y)) {
    delta <- (diff(range_x) - diff(range_y)) / 2
    range_y[1] <- range_y[1] - delta
    range_y[2] <- range_y[2] + delta
  } else {
    delta <- (diff(range_y) - diff(range_x)) / 2
    range_x[1] <- range_x[1] - delta
    range_x[2] <- range_x[2] + delta
  }

  # to define old par on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # define plotting space
  par(mfrow = c(1, 2))

  # initialize empty plot
  plot(NA,
    xlab = "Local - East - Coord.",
    ylab = "Local - North - Coord.", xlim = range_x,
    ylim = range_y, lwd = 3, main = "North-East coordinates"
  )

  # define covmat
  cov_mat_emu_to_plot <- x$Cov.Nav[[emu_to_plot]]
  cov_mat_position <- cov_mat_emu_to_plot[1:2, 1:2, 1:dim(cov_mat_emu_to_plot)[3]]

  # define traj
  traj_n <- x$traj.fused[[emu_to_plot]]$trajectory$x_N
  traj_e <- x$traj.fused[[emu_to_plot]]$trajectory$x_E
  traj_emu <- cbind(traj_e, traj_n)

  # define time interval
  t_diff <- x$t[2] - x$t[1]
  obs_freq <- time_interval / t_diff

  # extract all points from traj and from covmat
  length_seq <- dim(traj_emu)[1]
  obs_vector <- seq(from = 2, to = length_seq, by = obs_freq)

  if (plot_CI == TRUE) {
    # 95% interval
    for (i in obs_vector) {
      mu <- traj_emu[i, ]
      sigma <- cov_mat_position[, , i]


      # polygon(car::ellipse(center = mu,
      #                      shape = sigma,
      #                      radius = (sqrt(qchisq(.2, df=2))),
      #                      draw = FALSE),
      #         col = col_95,
      #         border= col_95_brd)
    }

    # 50% interval
    for (i in obs_vector) {
      mu <- traj_emu[i, ]
      sigma <- cov_mat_position[, , i]
      # polygon(car::ellipse(center = mu,
      #                      shape = sigma,
      #                      radius = (sqrt(qchisq(.01, df=2))),
      #                      draw = FALSE),
      #         col = col_50,
      #         border= col_50_brd)
    }
  }

  # plot true trajectory
  lines(traj$x_E, traj$x_N,
    col = true_col, type = "l",
    xlab = "Local - East - Coord.",
    ylab = "Local - North - Coord.", xlim = range_x,
    ylim = range_y, lwd = 3, main = "North-East coordinates"
  )

  # plot fused trajectory and store the mean of all lat-long coordinate in a mean coordinate vector
  for (i in 1:m) {
    if (i == 1) {
      currentxE <- x$traj.fused[[i]]$trajectory$x_E / m
      currentxN <- x$traj.fused[[i]]$trajectory$x_N / m
    } else {
      currentxE <- currentxE + x$traj.fused[[i]]$trajectory$x_E / m
      currentxN <- currentxN + x$traj.fused[[i]]$trajectory$x_N / m
    }
    lines(x$traj.fused[[i]]$trajectory$x_E, x$traj.fused[[i]]$trajectory$x_N, col = col_fused_trans)
  }

  # plot mean trajectory stored in above for-loop
  if (m > 1 && plot_mean_traj == T) {
    lines(currentxE, currentxN, col = col_fused_full, lwd = 2, lty = 2)
  }

  # plot the gps points on the left lat-long graph
  gps_data <- x$data.gps[[emu_to_plot]]
  nb_gps_obs <- ncol(gps_data)
  for (i in 1:nb_gps_obs) {
    points(gps_data[3, i], gps_data[2, i], col = col_fused_full, pch = 16, cex = 0.8)
  }

  # legend for Local North East Coordinates

  if (plot_CI == TRUE) {
    legend("bottomright",
      pch = c(NA, NA, NA, 16, 15, 15),
      lty = c(1, 1, 2, NA, NA, NA),
      lwd = c(2, 1, 2, NA, NA, NA),
      col = c(true_col, col_fused_trans, col_fused_full, col_fused_full, col_95, col_50),
      legend = c(
        "True trajectory", "Emulated trajectories",
        "Mean trajectory of emulated trajectories", "GPS North-East coordinates", "95% CI", "50% CI"
      ),
      cex = .7, bty = "n"
    )
  } else if (plot_CI == FALSE) {
    legend("bottomright",
      pch = c(NA, NA, NA, 16),
      lty = c(1, 1, 2, NA),
      lwd = c(2, 1, 2, NA),
      col = c(true_col, col_fused_trans, col_fused_full, col_fused_full),
      legend = c(
        "True trajectory", "Emulated trajectories",
        "Mean trajectory of emulated trajectories", "GPS North-East coordinates"
      ),
      cex = .7, bty = "n"
    )
  }

  # Second plot - Altitude

  # Empty plot
  plot(traj$time, -traj$x_D,
    col = "white", type = "l",
    xlab = "Time (sec)", lwd = 2.5,
    ylab = "Local - Up - Coord.", ylim = range_z, main = "Up coordinates"
  )

  # plot CI for altitude
  if (plot_CI == TRUE) {
    # extract covmat
    cov_mat_emu_to_plot <- x$Cov.Nav[[emu_to_plot]]

    # extract value var_z, time x and altitude y
    var_z <- cov_mat_emu_to_plot[3, 3, 1:dim(cov_mat_emu_to_plot)[3]]
    std_z <- sqrt(var_z)
    x_coor <- x$traj.fused[[emu_to_plot]]$trajectory$time
    y_coor <- -x$traj.fused[[emu_to_plot]]$trajectory$x_D

    # define normal quantile and plot polygon
    qnorm_val_95 <- qnorm(.975)
    qnorm_val_50 <- qnorm(.75)

    # plot polygon interval 95
    polygon(
      x = c(x_coor, rev(x_coor)),
      y = c(y_coor + qnorm_val_95 * -std_z, rev(y_coor + qnorm_val_95 * std_z)),
      col = col_95,
      border = col_95_brd
    )

    # plot polygon interval 95

    polygon(
      x = c(x_coor, rev(x_coor)),
      y = c(y_coor + qnorm_val_50 * -std_z, rev(y_coor + qnorm_val_50 * std_z)),
      col = col_50,
      border = col_50_brd
    )
  }

  # plot altitude over time
  lines(traj$time, -traj$x_D,
    col = true_col, type = "l",
    xlab = "Time (sec)", lwd = 2.5,
    ylab = "Local - Up - Coord.", ylim = range_z, main = "Up coordinates"
  )

  # plot all vertical trajectory for all emulations and store a mean vector of trajectory
  for (i in 1:m) {
    if (i == 1) {
      currentxU <- -x$traj.fused[[i]]$trajectory$x_D / m
    } else {
      currentxU <- currentxU - x$traj.fused[[i]]$trajectory$x_D / m
    }
    lines(x$traj.fused[[i]]$trajectory$time, -x$traj.fused[[i]]$trajectory$x_D, col = col_fused_trans)
  }

  # plot mean trajectory for vertical trajectories
  if (m > 1 && plot_mean_traj == T) {
    lines(x$traj.fused[[1]]$trajectory$time, currentxU, col = col_fused_full, lwd = 2, lty = 2)
  }

  # plot gps points for altitude graph
  for (i in 1:nb_gps_obs) {
    points(gps_data[1, i], -gps_data[4, i], col = col_fused_full, pch = 16, cex = 0.8)
  }

  # plot baro points
  if (plot_baro == T) {
    points(t(as.data.frame(x$data.baro[[emu_to_plot]])), col = baro_col, pch = 16, cex = 0.8)
  }

  # plot legend
  if (plot_CI == TRUE) {
    legend("topleft",
      pch = c(NA, NA, NA, 16, 16, 15, 15),
      lty = c(1, 1, 2, NA, NA, NA, NA),
      lwd = c(2, 1, 2, NA, NA, NA, NA),
      col = c(true_col, col_fused_trans, col_fused_full, col_fused_full, baro_col, col_95, col_50),
      legend = c(
        "True trajectory", "Emulated trajectories",
        "Mean trajectory of emulated trajectories", "GPS Altitude",
        "Barometer Altitude", "95% CI", "50% CI"
      ),
      cex = .7, bty = "n"
    )
  } else if (plot_CI == FALSE) {
    legend("topleft",
      pch = c(NA, NA, NA, 16, 16),
      lty = c(1, 1, 2, NA, NA),
      lwd = c(2, 1, 2, NA, NA),
      col = c(true_col, col_fused_trans, col_fused_full, col_fused_full, baro_col),
      legend = c(
        "True trajectory", "Emulated trajectories",
        "Mean trajectory of emulated trajectories", "GPS Altitude",
        "Barometer Altitude"
      ),
      cex = .7, bty = "n"
    )
  }

  # reset plotting space
  par(mfrow = c(1, 1))

  # error analysis
  if (error_analysis == TRUE) {
    # identify true trajectory
    true_traj <- cbind(traj$time, traj$x_N, traj$x_E, -traj$x_D)

    # create list that will holds values of squarred error for all emulated trajectory
    squared_error_list <- list()

    # store a max value
    max_value <- 0

    # for all emulated trajectories calculate the squared error
    for (j in seq(m)) {
      emu_traj <- x$traj.fused[[j]]$trajectory
      emu_traj <- cbind(emu_traj$time, emu_traj$x_N, emu_traj$x_E, -emu_traj$x_D)

      # create empty vector to store the squared difference
      diff_position_sum_squared <- c()

      # go over all timestep t
      for (i in seq(dim(true_traj)[1])) {
        diff <- true_traj[i, 2:4] - emu_traj[i, 2:4]

        # add to existing vector
        diff_position_sum_squared <- c(diff_position_sum_squared, sqrt(t(diff) %*% diff))

        # keep the max value
        max_value <- max(max_value, max(diff_position_sum_squared))
      }

      # store in list the vector of squared error corresponding to that emulated path
      squared_error_list[[j]] <- diff_position_sum_squared
    }

    # create empty vectors
    quantile_95 <- c()
    quantile_50 <- c()

    # define time interval
    # t_diff = x$t[2]-x$t[1]
    # obs_freq = time_interval_simu / t_diff

    # extract all points from traj and from covmat
    # length_seq = dim(traj_emu)[1]
    # obs_vector = seq(from = 2, to = length_seq, by = obs_freq)

    # for all time points in obs-vector
    for (t in 2:length(x$t_p)) {
      set.seed(seed)
      # isolate cov mat at time t
      my_cov_mat <- x$Cov.Nav[[emu_for_covmat]][1:3, 1:3, t]
      # simulate from a multinormal
      simulated_traj <- MASS::mvrnorm(n = nsim, mu = c(0, 0, 0), Sigma = my_cov_mat)

      # create vector of diff
      squared_diff_simu <- c()
      for (i in seq(dim(simulated_traj)[1])) {
        diff <- simulated_traj[i, ]
        squared_diff_simu <- c(squared_diff_simu, sqrt(t(diff) %*% diff))
      }
      quantile_95 <- c(quantile_95, quantile(squared_diff_simu, probs = .95))
      quantile_50 <- c(quantile_50, quantile(squared_diff_simu, probs = .50))
    }

    # empty plot with correct dimensions
    mymax <- max(max_value, quantile_50, quantile_95)
    plot(NA, ylim = c(0, mymax), xlim = range(x$traj.fused[[1]]$trajectory$time), xlab = "Time (sec)", ylab = "Position error (norm)") # main = expression(paste(sqrt(X^t*X), ' over Time')))

    # plot 95 % confidence interval
    my_x <- x$t_p[2:length(x$t_p)]
    polygon(
      x = c(my_x, rev(my_x)),
      y = c(rep(0, length(x$t_p) - 1), rev(quantile_95)),
      col = col_95,
      border = col_95_brd
    )

    # plot 50% confidence intervals
    polygon(
      x = c(my_x, rev(my_x)),
      y = c(rep(0, length(x$t_p) - 1), rev(quantile_50)),
      col = col_50,
      border = col_50_brd
    )

    # plot error lines
    for (k in seq(m)) {
      my_x <- seq(length(squared_error_list[[k]]))
      my_x <- my_x * t_diff
      lines(my_x, squared_error_list[[k]], col = col_traj_error)
    }

    # add legend
    legend("topleft",
      pch = c(NA, 15, 15),
      lty = c(1, NA, NA),
      lwd = c(1, NA, NA),
      pt.cex = c(NA, 2, 2),
      col = c(col_traj_error, col_95, col_50),
      legend = c("Emulated trajectories error", "95% CI", "50% CI"),
      cex = 1, bty = "n"
    )
  }





  # 3d plot
  if (plot3d == T) {
    # 3d plot
    traj <- x$traj.ref$trajectory

    # define p
    p <- plotly::plot_ly()

    # add true trajectory
    p <- p %>% add_trace(
      x = ~x_E, y = ~x_N, z = ~ -x_D, type = "scatter3d",
      mode = "lines", line = list(color = true_col, width = 5, size = 0),
      data = traj, name = "Reference trajectory"
    )

    # add emulated trajectories
    for (i in 1:m) {
      p <- p %>%
        add_trace(
          x = ~x_E, y = ~x_N, z = ~ -x_D,
          line = list(color = col_fused_full, width = 1), type = "scatter3d",
          mode = "lines", data = x$traj.fused[[i]]$trajectory,
          name = paste("Fused trajectory #", i, sep = "")
        )
    }

    # define coordinates
    p <- p %>%
      plotly::layout(
        scene = list(
          xaxis = list(title = "Local - East"),
          yaxis = list(title = "Local - North"),
          zaxis = list(title = "Local - Up"),
          aspectmode = "data"
        )
      )

    # store gps points in mygps object
    mygps <- as.data.frame(t(x$data.gps[[1]]))
    dimnames(mygps)[[2]] <- c("time", "x", "y", "z", "vx", "vy", "vz")

    # add the gps points
    p <- p %>% add_markers(
      x = ~y, y = ~x, z = ~ -z,
      marker = list(color = col_fused_full, size = 2),
      data = mygps, name = "GPS"
    )

    # add baro points
    traj_xy <- traj[, c(1:3)]
    baro_z <- as.data.frame(t(as.data.frame(x$data.baro[[emu_to_plot]])))
    colnames(baro_z) <- c("time", "z")
    my_df <- merge(traj_xy, baro_z)
    p <- p %>% add_markers(
      x = ~x_E, y = ~x_N, z = ~z,
      marker = list(color = "black", size = 2),
      data = my_df, name = "Barometer"
    )
    p
  }
}
