#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @param traj \code{traj} object
#' @param timing \code{timing} object
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#' @importFrom stats spline
check_traj <- function(traj, timing) {
  if (!inherits(traj, "trajectory")) {
    stop("\'traj\' must be of class \'trajectory\' created via \'make_trajectory\' function.")
  }

  t <- traj$trajectory$time
  t_0 <- t[1]
  t_end <- tail(t, n = 1)

  # Check/apply nav.start----
  if (is.null(timing$nav.start)) {
    timing$nav.start <- t_0
  } else if (timing$nav.start >= t_end) {
    stop("timing$nav.start can not be greater than final time of traj.")
  } else {
    traj$trajectory <- traj$trajectory[t >= timing$nav.start, ]
    t <- traj$trajectory$time
    # traj = traj[,t>=timing$nav.start]
    # t = traj[1,]
    t_0 <- t[1]
  }

  # Check/apply nav.end----
  if (is.null(timing$nav.end)) {
    timing$nav.end <- t_end
  } else if (timing$nav.end <= t_0) {
    stop("timing$nav.end can not be smaller than initial time of traj.")
  } else {
    traj$trajectory <- traj$trajectory[t <= timing$nav.end, ]
    t <- traj$trajectory$time
    t_end <- tail(t, n = 1)
  }

  # Interpolating (spline) traj if necessary----
  dt <- diff(t)
  dt_mean <- mean(dt)
  dt_jitter_max <- max(abs(dt_mean - dt))

  if (dt_jitter_max > 1e-9 || abs(1 / dt_mean - timing$freq.imu) > 1e-9) {
    t_reg <- seq(from = t[1], to = tail(t, n = 1), by = 1 / timing$freq.imu)
    trajectory_reg <- matrix(0, nrow = length(t_reg), ncol = 7)
    trajectory_reg[, 1] <- t_reg
    trajectory_reg <- make_trajectory(data = trajectory_reg, system = "ned")
    for (i in 2:7) {
      trajectory_reg$trajectory[, i] <- spline(x = t, y = traj$trajectory[, i], xout = t_reg)$y
    }
    warning("traj was interpolated to reqularize the time or match the frequency to timing$freq.imu.")
  } else {
    trajectory_reg <- traj
  }

  # Checking for Euler angles singularity----
  if (any(abs(trajectory_reg$trajectory$pitch) >= (pi / 2 - 1e-3))) {
    stop("Since Euler angles are used for attitude representation, pitch angles of close to pi/2 causing singularity issues must be avoided.")
  }

  out <- list("traj" = trajectory_reg, "timing" = timing)
  return(out)
}
