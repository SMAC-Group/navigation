#' @title Print a \code{sensor} object parameters (name, frequency and error model)
#' @description Print method for a \code{sensor} object
#' @return Print the \code{sensor} object name and specifications in the console.
#' @param x A \code{sensor} object.
#' @param ... Further arguments passed to or from other methods.
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
#'
#' @examples
#' # IMU:
#' imu.freq <- 250
#' acc.mdl <- WN(sigma2 = 1.535466e-04) + RW(gamma2 = 1.619511e-10)
#' gyr.mdl <- WN(sigma2 = 1.711080e-03) + RW(gamma2 = 1.532765e-13)
#' imu.mdl <- make_sensor(
#'   name = "imu",
#'   frequency = imu.freq,
#'   error_model1 = acc.mdl,
#'   error_model2 = gyr.mdl
#' )
#' print(imu.mdl)
#'
print.sensor <- function(x, ...) {
  cat("Senor name is: ", x$name, "\b.\n\n")
  cat("Sensor frequency is: ", x$frequency, "Hz.\n\n")
  cat("Sensor error model:\n")
  print(x$error_model) # To be modified to print only the relevant information.
}
