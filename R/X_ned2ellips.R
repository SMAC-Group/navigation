#' @title Transform position from NED to ellipsoidal coordinates
#' @description Transform position from a fixed Cartesian NED frame to ellipsoidal coordinates
#' @param x An object of class \code{trajectory} in "NED" system or a matrix of position data with x_N, x_E, and x_D
#' @param x_o Origin of the fixed Cartesian NED frame expressed in ellipsoidal coordinates
#' @return An object of class \code{trajectory} in "ellipsoidal" system or a matrix of position data with latitude, longitude, and altitude, according to the type of input \code{x}
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
#'
#' @examples
#'
#' data("example_1_traj_ned")
#' traj_ned <- make_trajectory(example_1_traj_ned, system = "ned")
#' plot(traj_ned)
#' traj_ellips <- X_ned2ellips(traj_ned, x_o = example_1_traj_ellipsoidal[1, -1])
#' plot(traj_ellips, threeD = FALSE)
#' plot(traj_ellips, threeD = TRUE)
#'
X_ned2ellips <- function(x, x_o = NULL) {
  # Check input types and condidtency
  if (inherits(x = x, what = "trajectory")) {
    if (x$system != "ned") {
      stop("X_ned2ellips requires an input trajectory of \'ned\' system.")
    }
    x_ned <- t(cbind(
      x$trajectory$x_N,
      x$trajectory$x_E,
      x$trajectory$x_D
    ))
  } else {
    x_ned <- x
  }

  # Check x_o
  if (is.null(x_o)) {
    x_o <- c(0, 0, 0)
    warning("Since not provided, x_o was set to c(0,0,0).")
  }

  # Calculate output
  x_ecef <- X_ned2ecef(x_ned, x_o)
  x_ellips <- X_ecef2ellips(x_ecef)

  # Return output based on input type
  if (inherits(x = x, what = "trajectory")) {
    N <- dim(x$trajectory)[2]
    if (N == 4) {
      data <- cbind(x$trajectory$time, t(x_ellips))
    } else if (N == 7) {
      data <- cbind(x$trajectory$time, t(x_ellips), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw)
    } else if (N == 10) {
      data <- cbind(x$trajectory$time, t(x_ellips), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw, x$trajectory$v_N, x$trajectory$v_E, x$trajectory$v_D)
    } else {
      stop("Inconsistent input of type \'trajectory\'.")
    }
    out <- make_trajectory(data = data, system = "ellipsoidal", start_time = x$start_time, name = x$name)
    return(out)
  } else {
    rownames(x_ellips) <- c("lat", "lon", "alt")
    colnames(x_ellips) <- NULL
    return(x_ellips)
  }
}
