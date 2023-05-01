#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @param traj A \code{trajectory} object.
#' @noRd
complete_traj <- function(traj) {
  if (!inherits(traj, "trajectory")) {
    stop("\'traj\' must be of class \'trajectory\' created via \'make_trajectory\' function.")
  }

  N <- dim(traj$trajectory)[1]
  dt <- (tail(traj$trajectory$time, 1) - traj$trajectory$time[1]) / (N - 1) # traj must be regularized first (see check_traj.R)

  # v_NED
  v_tmp <- diff(traj$trajectory$x_N) / dt
  traj$trajectory$v_N <- c(v_tmp, tail(v_tmp, 1))

  v_tmp <- diff(traj$trajectory$x_E) / dt
  traj$trajectory$v_E <- c(v_tmp, tail(v_tmp, 1))

  v_tmp <- diff(traj$trajectory$x_D) / dt
  traj$trajectory$v_D <- c(v_tmp, tail(v_tmp, 1))

  return(traj)
}
