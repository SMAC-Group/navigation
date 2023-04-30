#' @title Construct a \code{timing} object
#' @description Construct a \code{timing} object controlling the timing and frequencies for navigation, making sure about the consistency and feasibility of provided information.
#' @param nav.start Time at which navigation starts
#' @param nav.end Time at which navigation ends
#' @param freq.imu Frequency of generated IMU data (and hence that of navigation)
#' @param freq.gps Frequency of generated GPS data
#' @param freq.baro Frequency of generated Baro data
#' @param gps.out.start Time at which GPS outage starts
#' @param gps.out.end Time at which GPS outage ends
#' @return An object of class \code{timing} containing sensor name and its additive error model along with the frequency associated to that model
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @examples
#' timing <- make_timing(
#'   nav.start = 0,
#'   nav.end = 50,
#'   freq.imu = 10,
#'   freq.gps = 1,
#'   freq.baro = 1e-5,
#'   gps.out.start = 25.1,
#'   gps.out.end = 45
#' )
#'
make_timing <- function(nav.start = NULL, nav.end = NULL, freq.imu = NULL, freq.gps = NULL, freq.baro = NULL, gps.out.start = NULL, gps.out.end = NULL) {
  default_imu_freq <- 100
  default_gps_freq <- 1
  max_gps_freq <- 10
  default_baro_freq <- 10

  # Check IMU frequency----
  if (is.null(freq.imu)) {
    freq.imu <- default_imu_freq
    warning(paste("Since not provided, freq.imu was set to the default value of", freq.imu, "Hz."))
  } else if (freq.imu < 0) {
    warning("freq.imu must be positive.")
  }

  # Check GPS frequency----
  if (is.null(freq.gps)) {
    if (freq.imu > default_gps_freq) {
      freq.gps <- default_gps_freq
      warning(paste("Since not provided, freq.gps was set to the default value of", freq.gps, "Hz."))
    } else {
      freq.gps <- freq.imu
      warning(paste("Since not provided, freq.gps was set to freq.imu =", freq.imu, "Hz."))
    }
  } else if (freq.gps <= 0) {
    stop("freq.gps must be positive.")
  } else if (freq.gps > max_gps_freq && freq.gps <= freq.imu) {
    warning(paste("A value above", max_gps_freq, "Hz for freq.gps is not realistic."))
  } else if (freq.gps > freq.imu) {
    freq.gps <- freq.imu
    warning(paste("freq.gps was lowered down to freq.imu =", freq.imu, "Hz."))
    if (freq.gps > max_gps_freq) {
      warning(paste("A value above", max_gps_freq, "Hz for freq.gps is not realistic."))
    }
  }

  # Check Baro frequency----
  if (is.null(freq.baro)) {
    if (freq.imu > default_baro_freq) {
      freq.baro <- default_baro_freq
      warning(paste("Since not provided, freq.baro was set to the default value of", freq.baro, "Hz."))
    } else {
      freq.baro <- freq.imu
      warning(paste("Since not provided, freq.baro was set to freq.imu =", freq.imu, "Hz."))
    }
  } else if (freq.baro <= 0) {
    stop("freq.baro must be positive.")
  } else if (freq.baro > freq.imu) {
    freq.baro <- freq.imu
    warning(paste("freq.baro was lowered down to freq.imu =", freq.imu, "Hz."))
  }

  # Check if nav.start < nav.end----
  if (isTRUE(nav.start > nav.end)) {
    stop("nav.start cannot be greater than nav.end.")
  }

  if (length(gps.out.start) != length(gps.out.end)) {
    stop("gps.out.start and gps.out.end must have the same length")
  }

  # Check gps.out.start and gps.out.end consistency----
  for (j in 1:length(gps.out.start)) {
    if (isTRUE(gps.out.start[j] > gps.out.end[j])) {
      stop("gps.out.start cannot be greater than gps.out.end.")
    }
  }

  # Create timing object----
  out <- list(
    "nav.start" = nav.start,
    "nav.end" = nav.end,
    "freq.imu" = freq.imu,
    "freq.gps" = freq.gps,
    "freq.baro" = freq.baro,
    "gps.out.start" = gps.out.start,
    "gps.out.end" = gps.out.end
  )
  class(out) <- "timing"
  return(out)
}
