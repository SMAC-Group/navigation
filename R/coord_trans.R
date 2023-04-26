#' @title Transforms position from ellipsoidal to ned coordinates
#' @description Transforms position from ellipsoidal coordinates to a fixed Cartesian ned frame
#' @param x An object of class \code{trajectory} in "ellipsoidal" system or a matrix of position data with latitude, longitude, and altitude
#' @param x_o Origin of the fixed Cartesian ned frame expressed in ellipsoidal coordinates
#' @return An object of class \code{trajectory} in "ned" system or a matrix of position data with x_N, x_E, and x_D, according to the type of input \code{x}
#' @export
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
X_ellips2ned = function(x, x_o=NULL ) {

  # Check input types and condidtency
  if (inherits(x = x, what = "trajectory")) {
    if (x$system != "ellipsoidal") {
      stop("X_ellips2ned requires an input trajectory of \'ellipsoidal\' system.")
    }
    x_ellips = t(cbind(x$trajectory$lat,
                       x$trajectory$lon,
                       x$trajectory$alt))
  } else {
    x_ellips = x
  }

  # Check x_o
  if (is.null(x_o)) {
    x_o = c(0,0,0)
    warning("Since not provided, x_o was set to c(0,0,0).")
  }

  # Calculate output
  x_ecef = X_ellips2ecef(x_ellips)
  x_ned = X_ecef2ned(x_ecef, x_o)


  # Return output based on input type
  if (inherits(x = x, what = "trajectory")) {
    N = dim(x$trajectory)[2]
    if (N==4) {
      data = cbind(x$trajectory$time, t(x_ned))
    } else if (N==7) {
      data = cbind(x$trajectory$time, t(x_ned), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw)
    } else if (N==10) {
      data = cbind(x$trajectory$time, t(x_ned), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw, x$trajectory$v_N, x$trajectory$v_E, x$trajectory$v_D)
    } else {
      stop("Inconsistent input of type \'trajectory\'.")
    }
    out = make_trajectory(data = data, system = "ned", start_time = x$start_time, name = x$name)
    return(out)
  } else {
    rownames(x_ned) = c("x_N","x_E","x_D")
    colnames(x_ned) = NULL
    return(x_ned)
  }
  rownames(y) = c("x_N","x_E","x_D")
  colnames(y) = NULL
  return(y)
}


#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
X_ellips2ecef = function( x ) {

  if (!is.matrix(x)) {
    x = as.matrix(x)
  }

  # Function's constants (WGS-84)
  WGS84 = WGS84_datum()

  a = WGS84$a # [m]
  b = WGS84$b # [m]

  # Output calculation
  phi = x[1,]
  lambda = x[2,]
  h = x[3,]

  cphi = cos(phi)
  sphi = sin(phi)
  clambda = cos(lambda)
  slambda = sin(lambda)

  N = a^2/sqrt((a*cphi)^2+(b*sphi)^2)

  y = rbind((N+h)*cphi*clambda,
            (N+h)*cphi*slambda,
            ((b/a)^2*N+h)*sphi)
}


#' @title Transforms ECEF to NED coordinates
#' @description Transforms ECEF to local-level NED coordinates centered at \code{x_0}
#' @param x a 3 x n matrix with ECEF coordinates to be converted
#' @param x_o the latitude, longitude, altitude of the desired local level origin
#' @return \code{x} converted to the local level
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#' @export
#' 
#'
X_ecef2ned = function( x, x_o ) {

  ## Output calculation
  l_phi = x_o[1]      # latitude [rad]
  l_lambda = x_o[2]   # longitude [rad]
  l_h = x_o[3]        # height [m]
  r_el = X_ellips2ecef( c(l_phi, l_lambda, l_h) ) # r_el is the position vector from l frame (local, NED) origin to e frame origin

  Cle = Cnefunc( l_phi, l_lambda )   # Cle is the rotation matrix to transform vectors from l frame (local, NED) to e frame

  r_el = matrix(data = r_el ,nrow = 3, ncol = dim(x)[2], byrow = FALSE)
  y = t(Cle) %*% (x - r_el)

}


#' @title Compute \eqn{C^e_l} 
#' @description Compute \eqn{C^e_l}, the rotation matrix to transform vectors from l frame (local, NED) to e frame
#' @param phi latitude in radians of the origin of the local level
#' @param lambda longitude in radians of the origin of the local level
#' @return The rotation matrix \eqn{C^e_l}
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
#' @noRd
Cnefunc = function( phi, lambda ) {

  cp = cos(phi)
  sp = sin(phi)

  cl = cos(lambda)
  sl = sin(lambda)

  Cne = rbind(c(-sp*cl, -sl, -cp*cl),
              c(-sp*sl,	 cl, -cp*sl),
              c(cp,      0,  -sp   ))

}


# ----------------------------------------------------------
#' @title Transforms position from ned to ellipsoidal coordinates
#' @description Transforms position from a fixed Cartesian ned frame to ellipsoidal coordinates
#' @param x An object of class \code{trajectory} in "ned" system or a matrix of position data with x_N, x_E, and x_D
#' @param x_o Origin of the fixed Cartesian ned frame expressed in ellipsoidal coordinates
#' @return An object of class \code{trajectory} in "ellipsoidal" system or a matrix of position data with latitude, longitude, and altitude, according to the type of input \code{x}
#' @export
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
X_ned2ellips = function(x, x_o=NULL ) {

  # Check input types and condidtency
  if (inherits(x = x, what = "trajectory")) {
    if (x$system != "ned") {
      stop("X_ned2ellips requires an input trajectory of \'ned\' system.")
    }
    x_ned = t(cbind(x$trajectory$x_N,
                    x$trajectory$x_E,
                    x$trajectory$x_D))
  } else {
    x_ned = x
  }

  # Check x_o
  if (is.null(x_o)) {
    x_o = c(0,0,0)
    warning("Since not provided, x_o was set to c(0,0,0).")
  }

  # Calculate output
  x_ecef = X_ned2ecef(x_ned, x_o)
  x_ellips = X_ecef2ellips(x_ecef)

  # Return output based on input type
  if (inherits(x = x, what = "trajectory")) {
    N = dim(x$trajectory)[2]
    if (N==4) {
      data = cbind(x$trajectory$time, t(x_ellips))
    } else if (N==7) {
      data = cbind(x$trajectory$time, t(x_ellips), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw)
    } else if (N==10) {
      data = cbind(x$trajectory$time, t(x_ellips), x$trajectory$roll, x$trajectory$pitch, x$trajectory$yaw, x$trajectory$v_N, x$trajectory$v_E, x$trajectory$v_D)
    } else {
      stop("Inconsistent input of type \'trajectory\'.")
    }
    out = make_trajectory(data = data, system = "ellipsoidal", start_time = x$start_time, name = x$name)
    return(out)
  } else {
    rownames(x_ellips) = c("lat","lon","alt")
    colnames(x_ellips) = NULL
    return(x_ellips)
  }

}


#' @title X_ned2ecef
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
X_ned2ecef = function( x, x_o ) {

  if (!is.matrix(x)) {
    x = as.matrix(x)
  }

  ## Output calculation
  l_phi = x_o[1]      # latitude [rad]
  l_lambda = x_o[2]   # longitude [rad]
  l_h = x_o[3]        # height [m]
  r_el = X_ellips2ecef( c(l_phi, l_lambda, l_h) ) # r_el is the position vector from l frame (local, NED) origin to e frame origin

  Cle = Cnefunc( l_phi, l_lambda )   # Cle is the rotation matrix to transform vectors from l frame (local, NED) to e frame

  r_el = matrix(data = r_el ,nrow = 3, ncol = dim(x)[2], byrow = FALSE)

  y = r_el + Cle %*% x

}


#' @title Convert ECEF to ellipsoidal coordinates
#' @description Convert ECEF to ellipsoidal (latitude, longitude, altitude, in WGS84) coordinates
#' @param X_ec a 3 x n matrix containing the ECEF coordinatesto be converted
#' @param ignoreAtan2Flag A \code{boolean} value indicating if Atan2Flag should be ignored.
#' @return a matrix containing the WGS84 latitude, longitude, altitude converted coordinates
#' @export
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
X_ecef2ellips = function ( X_ec, ignoreAtan2Flag = FALSE ) {


  # Function's constants (WGS-84)
  WGS84 = WGS84_datum()

  a = WGS84$a # [m]
  b = WGS84$b # [m]
  e2 = 1-(b/a)^2
  ep2 = (a/b)^2-1

  ## Output calculation
  x1 = X_ec[1,]
  x2 = X_ec[2,]
  x3 = X_ec[3,]

  # To avoid having atan(0/0)
  p = sqrt(x1^2 + x2^2)
  x1[p==0] = 1e-50
  p[p==0] = 1e-50


  if (ignoreAtan2Flag) {
    lambda = atan(x2/x1)
    psi = atan(x3*a/(p*b))
    phi = atan((x3+ep2*b*sin(psi)^3)/(p-e2*a*cos(psi)^3))
  } else {
    lambda = atan2(x2,x1)
    psi = atan2(x3*a,p*b)
    phi = atan2((x3+ep2*b*sin(psi)^3),(p-e2*a*cos(psi)^3))
  }
  N = a^2/sqrt((a*cos(phi))^2+(b*sin(phi))^2)
  h = p/cos(phi)-N

  X_el = rbind(phi,
               lambda,
               h)
}
