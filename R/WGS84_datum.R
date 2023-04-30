#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
WGS84_datum <- function() {
  # WGS84 datum

  WGS84 <- list(
    "a" = 6378137.0, # Semi-major axis [m]
    "w" = 7292115.0e-11, # Angular velocity of the Earth [rad/s]
    "GM" = 3986004.418e8, # Eart's Gravitational constant [m^3/s^2]
    "fInv" = 298.257223563, # Reciprocal of flattening
    "f" = 1 / 298.257223563, # Flattening
    "b" = 6356752.3142, # Semi-minor Axis [m]
    "e" = 8.1819190842622e-2, # First Eccentricity
    "ePrime" = 8.2094437949696e-2, # Second Eccentricity
    "gamma_e" = 9.7803253359, # Theoretical (Normal) Gravity at the Equator (on the Ellipsoid) [m/s^2]
    "gamma_p" = 9.8321849378, # Theoretical (Normal) Gravity at the pole (on the Ellipsoid) [m/s^2]
    "k" = 0.00193185265241, # Theoretical (Normal) Gravity Formula Constant
    "m" = 0.00344978650684
  ) # m=w2a2b/GM
  return(WGS84)
}
