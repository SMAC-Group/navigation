#' @title Construct a \code{sensor} object
#' @description Construct a \code{sensor} object for IMU, GPS, and Baro from error model of class \code{ts.model}
#' @param name Name of the sensor
#' @param frequency Frequency associated with the error model
#' @param error_model1 Error model of class \code{ts.model} for either accelerometer (as part of imu), horizontal components of GPS position, or Barometer
#' @param error_model2 Error model of class \code{ts.model} for either gyroscope (as part of imu) or vertical component of GPS position
#' @param error_model3 Error model of class \code{ts.model} for horizontal components of GPS velocity
#' @param error_model4 Error model of class \code{ts.model} for vertical component of GPS velocity
#' @param error_data1 Vector of error observations.
#' @return An object of class \code{sensor} containing sensor name and its additive error model along with the frequency associated to that model
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
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
#'
#' # GPS:
#' gps.freq <- 1
#' gps.mdl.pos.hor <- WN(sigma2 = 2^2)
#' gps.mdl.pos.ver <- WN(sigma2 = 4^2)
#' gps.mdl.vel.hor <- WN(sigma2 = 0.04^2)
#' gps.mdl.vel.ver <- WN(sigma2 = 0.06^2)
#' gps.mdl <- make_sensor(
#'   name = "gps", frequency = gps.freq,
#'   error_model1 = gps.mdl.pos.hor,
#'   error_model2 = gps.mdl.pos.ver,
#'   error_model3 = gps.mdl.vel.hor,
#'   error_model4 = gps.mdl.vel.ver
#' )
#'
#' # Baro:
#' baro.freq <- 1
#' baro.mdl <- WN(sigma2 = 0.5^2)
#' baro.mdl <- make_sensor(
#'   name = "baro",
#'   frequency = baro.freq,
#'   error_model1 = baro.mdl
#' )
#'
make_sensor <- function(name, frequency = 1, error_model1 = NULL, error_model2 = NULL, error_model3 = NULL, error_model4 = NULL, error_data1 = NULL) {
  # Check frequency
  if (is.null(frequency)) {
    frequency <- 1
    warning("Senor frequency is set to the default value of 1 Hz.")
  } else if (frequency <= 0) {
    stop("Sensor frequency must be a positive number")
  }

  # check names and assign models
  if (name == "gps") {
    if (!inherits(error_model1, "ts.model")) {
      stop("Sensor error model must be of class \'ts.model\' (created using GMWM package).")
    }

    check_sensor_model(name, error_model1)
    check_sensor_model(name, error_model2)
    check_sensor_model(name, error_model3)
    check_sensor_model(name, error_model4)

    error_model <- list("pos.hor" = error_model1, "pos.ver" = error_model2, "vel.hor" = error_model3, "vel.ver" = error_model4)

    # create the "sensor" object
    out <- list(
      "name" = name,
      "frequency" = frequency,
      "error_model" = error_model
    )
  } else if (name == "baro") {
    if (!inherits(error_model1, "ts.model")) {
      stop("Sensor error model must be of class \'ts.model\' (created using GMWM package).")
    }

    check_sensor_model(name, error_model1)

    error_model <- error_model1

    # create the "sensor" object
    out <- list(
      "name" = name,
      "frequency" = frequency,
      "error_model" = error_model
    )
  } else if (name == "imu") {
    if (!is.null(error_model1) && !is.null(error_model2) && is.null(error_data1)) {
      good <- FALSE

      if (inherits(error_model1, "list")) {
        if ("X" %in% names(error_model1) && "Y" %in% names(error_model1) && "Z" %in% names(error_model1)) {
          check_sensor_model(name, error_model1$X)
          check_sensor_model(name, error_model1$Y)
          check_sensor_model(name, error_model1$Z)

          error_model_acc <- error_model1
        }
      } else if (inherits(error_model1, "ts.model")) {
        check_sensor_model(name, error_model1)

        error_model_acc <- list()
        error_model_acc$X <- error_model1
        error_model_acc$Y <- error_model1
        error_model_acc$Z <- error_model1

        good <- TRUE
      }

      if (inherits(error_model2, "list")) {
        if ("X" %in% names(error_model2) && "Y" %in% names(error_model2) && "Z" %in% names(error_model2)) {
          check_sensor_model(name, error_model2$X)
          check_sensor_model(name, error_model2$Y)
          check_sensor_model(name, error_model2$Z)

          error_model_gyro <- error_model2

          good <- TRUE
        }
      } else if (inherits(error_model2, "ts.model")) {
        check_sensor_model(name, error_model2)

        error_model_gyro <- list()
        error_model_gyro$X <- error_model2
        error_model_gyro$Y <- error_model2
        error_model_gyro$Z <- error_model2

        good <- TRUE
      }

      if (good == FALSE) {
        stop("error_model1 and error_model2 model must be either of class \'ts.model\' (created using GMWM package) or a list containing X Y Z items of class \'ts.model\'")
      }

      error_model <- list("acc" = error_model_acc, "gyr" = error_model_gyro)

      # create the "sensor" object
      out <- list(
        "name" = name,
        "frequency" = frequency,
        "error_model" = error_model
      )
    } else if (!is.null(error_data1)) {
      if (!inherits(error_data1, "imu")) {
        stop("error_data1 must inherit from an imu object")
      }

      if (frequency != attr(error_data1, "freq")) {
        warning(paste("error_data1 has a different sampling frequency wrt provided frequency: ", frequency, "vs", attr(error_data1, "freq")))
      }

      error_data <- error_data1

      # create the "sensor" object
      out <- list(
        "name" = name,
        "frequency" = frequency,
        "error_data" = error_data
      )
    } else {
      stop("must provide ethier a sensor model or some error data")
    }
  } else {
    stop("The only supported sensor names are: \'imu\', \'gps\', and \'baro\'.")
  }

  return(out)
}








check_sensor_model <- function(name, mdl) {
  n.WN <- sum(mdl$desc == "WN")
  n.RW <- sum(mdl$desc == "RW")
  n.DR <- sum(mdl$desc == "DR")
  n.GM <- sum(mdl$desc == "GM")

  n.AR1 <- sum(mdl$desc == "AR1")
  n.ARMA <- sum(mdl$desc == "ARMA")
  n.QN <- sum(mdl$desc == "QN")

  # Check WN for all sensors
  if (name != "imu" && n.WN != 1) {
    stop("All non-imu sensor error models must contain one WN process.")
  }

  # Check other processes for GPS and Baro
  if (sum(name == c("gps", "baro")) > 0) {
    if ((n.RW + n.DR + n.GM + n.AR1 + n.ARMA + n.QN) > 0) {
      warning("For GPS and Baro, only WN is supported. All the other processes (RW, DR, AR1, ARMA, and QN) will be ignored in filtering, but not in sensor data generation.")
    }
  }

  # Check other processes for IMU
  if (name == "imu") {
    # RW
    if (n.RW > 1) {
      stop("IMU error model can not contain more than one RW process.")
    }

    # DR
    if (n.DR > 1) {
      stop("IMU error model can not contain more than one DR process.")
    }

    # GM
    if (n.GM > 5) {
      stop("At the moment, no more than five GM processes are supported for the IMU within the filter.")
    }

    # other models
    if ((n.ARMA + n.QN) > 0) {
      warning("For IMU, ARMA, and QN processes are not supported and will be ignored in filtering, but not in sensor data generation.")
    }
  }
  return(0)
}
