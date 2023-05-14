#' @title Construct a \code{trajectory} object
#' @description Create a \code{trajectory} object from simple matrix input.
#' @param data        A multiple-column \code{matrix}. The first column corresponds to the measurment time (in seconds); columns 2, 3 and 4 corresponds to the positions (with the order lat, long and alt (in rad) if ellipsoidal coord or x_N, x_E and x_D for NED coord); columns 5, 6 and 7 (optional) corresponds to the attitude (with the order roll, pitch and yaw); columns 8, 9 and 10 (optional) corresponds to the velocity along the same axes are columns 2, 3 and 4.
#' @param system      A \code{string} corresponding to the coordinate system (possible choices: \code{ellipsoidal} or \code{ned}) considered.
#' @param start_time  A \code{string} (optional) corresponding to the start time for the trajectory.
#' @param name        A \code{string} (optional) corresponding to the name of the dataset.
#' @param ...         Additional arguments.
#' @return An object of class \code{trajectory}.
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @examples
#' n <- 100
#' dat <- cbind(
#'   seq(from = 0, to = 60 * 60, length.out = n),
#'   46.204391 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   6.143158 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   375 + cumsum(rnorm(n))
#' )
#' traj <- make_trajectory(data = dat, name = "My cool data")
#' traj
#' plot(traj)
make_trajectory <- function(data, system = "ellipsoidal", start_time = NULL, name = NULL, ...) {
  # Number of col and row
  size <- dim(data)

  # Check coord. system
  if (!(system %in% c("ellipsoidal", "ned"))) {
    stop("The input system must be either 'ellipsoidal' (for ellipsoidal coordinates, i.e. lat/long/alt) or 'ned' (for NED coordinates or Cartesian coordinates, i.e. x_N, x_E, x_D, see help for details.")
  }

  # Check dimension
  if (sum(size[2] == c(4, 7, 10)) == 0) {
    stop("The input data is incorrectly specified. It should have 4 (time + position), 7 (time + position + angles) or 10 (time + position + angles + velocity) colunms, see help for details.")
  }

  # Check start_time and date
  if (!is.null(start_time)) {
    start <- as.POSIXct(strptime(start_time, "%H:%M:%S"))
  } else {
    start <- as.POSIXct(strptime("00:00:00", "%H:%M:%S"))
  }

  # Construction time vector
  mytime <- format(start + data[, 1], "%H:%M:%OS6")

  # Case 1: position only
  if (size[2] == 4) {
    if (system == "ellipsoidal") {
      traj <- data.frame("time" = data[, 1], "lat" = data[, 2], "lon" = data[, 3], "alt" = data[, 4])
    } else {
      traj <- data.frame("time" = data[, 1], "x_N" = data[, 2], "x_E" = data[, 3], "x_D" = data[, 4])
    }
  }

  if (size[2] == 7) {
    if (system == "ellipsoidal") {
      traj <- data.frame(
        "time" = data[, 1], "lat" = data[, 2], "lon" = data[, 3], "alt" = data[, 4],
        "roll" = data[, 5], "pitch" = data[, 6], "yaw" = data[, 7]
      )
    } else {
      traj <- data.frame(
        "time" = data[, 1], "x_N" = data[, 2], "x_E" = data[, 3], "x_D" = data[, 4],
        "roll" = data[, 5], "pitch" = data[, 6], "yaw" = data[, 7]
      )
    }
  }

  if (size[2] == 10) {
    if (system == "ellipsoidal") {
      traj <- data.frame(
        "time" = data[, 1], "lat" = data[, 2], "lon" = data[, 3], "alt" = data[, 4],
        "roll" = data[, 5], "pitch" = data[, 6], "yaw" = data[, 7],
        "v_N" = data[, 8], "v_E" = data[, 9], "v_D" = data[, 10]
      )
    } else {
      traj <- data.frame(
        "time" = data[, 1], "x_N" = data[, 2], "x_E" = data[, 3], "x_D" = data[, 4],
        "roll" = data[, 5], "pitch" = data[, 6], "yaw" = data[, 7],
        "v_N" = data[, 8], "v_E" = data[, 9], "v_D" = data[, 10]
      )
    }
  }

  # Output object
  out <- list(trajectory = traj, system = system, start_time = start_time, name = name, time = mytime, ...)
  class(out) <- "trajectory"
  out
}
