#' @title Transform position from ellipsoidal to NED coordinates
#' @description Transform position from ellipsoidal coordinates to a fixed Cartesian NED frame
#' @param x An object of class \code{trajectory} in "ellipsoidal" system or a matrix of position data with latitude, longitude, and altitude
#' @param x_o Origin of the fixed Cartesian NED frame expressed in ellipsoidal coordinates
#' @return An object of class \code{trajectory} in "NED" system or a matrix of position data with x_N, x_E, and x_D, according to the type of input \code{x}
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
#' @examples
#'
#' library(navigation)
#' data("example_1_traj_ellipsoidal")
#' traj_ellips <- make_trajectory(example_1_traj_ellipsoidal, system = "ellipsoidal")
#' plot(traj_ellips)
#' plot(traj_ellips, threeD = TRUE)
#' traj_ned <- X_ellips2ned(traj_ellips, x_o = example_1_traj_ellipsoidal[1, -1])
#' plot(traj_ned)
#'
X_ellips2ned <- function(x, x_o = NULL) {
  # Check input types and condidtency
  if (inherits(x = x, what = "trajectory")) {
    if (x$system != "ellipsoidal") {
      stop("X_ellips2ned requires an input trajectory of \'ellipsoidal\' system.")
    }
    x_ellips <- t(cbind(
      x$trajectory$lat,
      x$trajectory$lon,
      x$trajectory$alt
    ))
  } else {
    x_ellips <- x
  }

  # Check x_o
  if (is.null(x_o)) {
    x_o <- c(0, 0, 0)
    warning("Since not provided, x_o was set to c(0,0,0).")
  }

  # Calculate output
  x_ecef <- X_ellips2ecef(x_ellips)
  x_ned <- X_ecef2ned(x_ecef, x_o)


  # Return output based on input type
  if (inherits(x = x, what = "trajectory")) {
    N <- dim(x$trajectory)[2]
    if (N == 4) {
      data <- cbind(x$trajectory$time, t(x_ned))
    } else if (N == 7) {
      data <- cbind(x$trajectory$time, t(x_ned), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw)
    } else if (N == 10) {
      data <- cbind(x$trajectory$time, t(x_ned), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw, x$trajectory$v_N, x$trajectory$v_E, x$trajectory$v_D)
    } else {
      stop("Inconsistent input of type \'trajectory\'.")
    }
    out <- make_trajectory(data = data, system = "ned", start_time = x$start_time, name = x$name)
    return(out)
  } else {
    rownames(x_ned) <- c("x_N", "x_E", "x_D")
    colnames(x_ned) <- NULL
    return(x_ned)
  }
  rownames(y) <- c("x_N", "x_E", "x_D")
  colnames(y) <- NULL
  return(y)
}
