#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @importFrom graphics par lines legend
#' @importFrom utils tail
#' @noRd
calc_err <- function(traj, gps_data, baro_data, printMaxErrors = TRUE) {
  warning("calc_err is just a temporary post process function for usage during development. It also does not fully comply with trajectory class. To be replaced by a proper post processor.")

  traj$ref <- t(cbind(
    traj$ref$trajectory$time,
    traj$ref$trajectory$x_N,
    traj$ref$trajectory$x_E,
    traj$ref$trajectory$x_D,
    traj$ref$trajectory$v_N,
    traj$ref$trajectory$v_E,
    traj$ref$trajectory$v_D,
    traj$ref$trajectory$roll,
    traj$ref$trajectory$pitch,
    traj$ref$trajectory$yaw
  ))

  #  Pos_err --------------------------------------
  t <- traj$calc$t
  err <- traj$calc$X[1:9, ] - traj$ref[2:10, ]
  err.pos.hor <- sqrt(err[1, ]^2 + err[2, ]^2)
  err.pos.tot <- sqrt(err[1, ]^2 + err[2, ]^2 + err[3, ]^2)
  err.pos.alt <- abs(err[3, ])
  SD.pos.tot <- sqrt(traj$calc$P[1, 1, ] + traj$calc$P[2, 2, ] + traj$calc$P[3, 3, ] + 2 * (traj$calc$P[1, 2, ] + traj$calc$P[1, 3, ] + traj$calc$P[2, 3, ]))
  SD.pos.hor <- sqrt(traj$calc$P[1, 1, ] + traj$calc$P[2, 2, ] + 2 * (traj$calc$P[1, 2, ]))
  SD.pos.alt <- sqrt(traj$calc$P[3, 3, ])
  dZgps <- traj$calc$dZgps
  dZbaro <- traj$calc$dZbaro

  # graphics.off()

  # plot(gps_data[3,],gps_data[2,],asp = 1)
  # plot(traj$ref[3,],traj$ref[2,],asp = 1)
  # plot(traj$calc$X[2,],traj$calc$X[1,],asp = 1)
  # plot(traj$calc$t,err[6,])
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 2))
  # 2D Traj
  plot(
    x = traj$ref[3, ],
    y = traj$ref[2, ],
    asp = 1,
    type = "l",
    col = "gray",
    xlab = "X_East [m]",
    ylab = "X_North [m]",
    main = "Trajectory (horizontal view)"
  )
  lines(
    x = traj$calc$X[2, ],
    y = traj$calc$X[1, ],
    col = "black"
  )
  lines(
    x = gps_data[3, ],
    y = gps_data[2, ],
    type = "p",
    pch = "x",
    col = "blue"
  )
  legend(x = "bottomright", lty = c(1, 1, NA), pch = c(NA, NA, "x"), legend = c("Ref.", "KF", "GPS"), col = c("gray", "black", "blue"))

  # Alt
  plot(
    x = t,
    y = -traj$ref[4, ],
    type = "l",
    col = "gray",
    xlab = "time [s]",
    ylab = "altitude [m]",
    main = "Altitude profile"
  )
  lines(
    x = t,
    y = -traj$calc$X[3, ],
    col = "black"
  )
  lines(
    x = gps_data[1, ],
    y = -gps_data[4, ],
    type = "p",
    pch = "x",
    col = "blue"
  )
  lines(
    x = baro_data[1, ],
    y = baro_data[2, ],
    type = "p",
    pch = "x",
    col = "red"
  )
  legend(x = "bottomright", lty = c(1, 1, NA, NA), pch = c(NA, NA, "x", "x"), legend = c("Ref.", "KF", "GPS", "Baro"), col = c("gray", "black", "blue", "red"))

  # Pos err
  plot(
    x = t,
    y = err.pos.hor,
    type = "l",
    col = "blue",
    lty = 1,
    xlab = "time [s]",
    ylab = "Position error [m]",
    main = "Position Error",
    ylim = range(c(err.pos.tot, SD.pos.tot))
  )
  lines(
    x = t,
    y = SD.pos.hor,
    col = "blue",
    lty = 2
  )
  lines(
    x = t,
    y = err.pos.tot,
    col = "black",
    lty = 1
  )
  lines(
    x = t,
    y = SD.pos.tot,
    col = "black",
    lty = 2
  )
  legend(x = "topleft", lty = c(1, 2, 1, 2), legend = c("Hor. Err.", "Hor. SD", "3D Err.", "3D SD"), col = c("blue", "blue", "black", "black"))

  # Alt err
  plot(
    x = t,
    y = err.pos.alt,
    type = "l",
    col = "black",
    lty = 1,
    xlab = "time [s]",
    ylab = "Altitude error [m]",
    main = "Altitude Error",
    ylim = range(c(err.pos.alt, SD.pos.alt))
  )
  lines(
    x = t,
    y = SD.pos.alt,
    col = "black",
    lty = 2
  )
  legend(x = "topleft", lty = c(1, 2), legend = c("Alt. Err.", "Alt. SD"), col = c("black", "black"))

  if (printMaxErrors) {
    print(paste("Maximum Position Error:", round(max(err.pos.tot), digits = 1), "m. Predicted:", round(max(SD.pos.tot), digits = 1), "m"))

    print(paste("Maximum Altitude Error:", round(max(err.pos.alt), digits = 1), "m. Predicted:", round(max(SD.pos.alt), digits = 1), "m"))
  }



  #  Vel_err --------------------------------------
  # t = traj$calc$t
  # err= traj$calc$X[1:9,] - traj$ref[2:10,]

  err.vel.hor <- sqrt(err[4, ]^2 + err[5, ]^2)
  err.vel.tot <- sqrt(err[4, ]^2 + err[5, ]^2 + err[6, ]^2)
  err.vel.ver <- abs(err[6, ])
  SD.vel.tot <- sqrt(traj$calc$P[4, 4, ] + traj$calc$P[5, 5, ] + traj$calc$P[6, 6, ] + 2 * (traj$calc$P[4, 5, ] + traj$calc$P[4, 6, ] + traj$calc$P[5, 6, ]))
  SD.vel.hor <- sqrt(traj$calc$P[4, 4, ] + traj$calc$P[5, 5, ] + 2 * (traj$calc$P[4, 5, ]))
  SD.vel.ver <- sqrt(traj$calc$P[6, 6, ])
  # graphics.off()

  # plot(gps_data[3,],gps_data[2,],asp = 1)
  # plot(traj$ref[3,],traj$ref[2,],asp = 1)
  # plot(traj$calc$X[2,],traj$calc$X[1,],asp = 1)
  # plot(traj$calc$t,err[6,])

  par(mfrow = c(1, 2))
  # Vel err
  plot(
    x = t,
    y = err.vel.hor,
    type = "l",
    col = "blue",
    lty = 1,
    xlab = "time [s]",
    ylab = "Velocity error [m/s]",
    main = "Velocity Error",
    ylim = range(c(err.vel.tot, SD.vel.tot))
  )
  lines(
    x = t,
    y = SD.vel.hor,
    col = "blue",
    lty = 2
  )
  lines(
    x = t,
    y = err.vel.tot,
    col = "black",
    lty = 1
  )
  lines(
    x = t,
    y = SD.vel.tot,
    col = "black",
    lty = 2
  )
  legend(x = "topleft", lty = c(1, 2, 1, 2), legend = c("Hor. Err.", "Hor. SD", "3D Err.", "3D SD"), col = c("blue", "blue", "black", "black"))

  # Ver Vel err
  plot(
    x = t,
    y = err.vel.ver,
    type = "l",
    col = "black",
    lty = 1,
    xlab = "time [s]",
    ylab = "Vertical velocity error [m/s]",
    main = "Vertical velocity Error",
    ylim = range(c(err.vel.ver, SD.vel.ver))
  )
  lines(
    x = t,
    y = SD.vel.ver,
    col = "black",
    lty = 2
  )
  legend(x = "topleft", lty = c(1, 2), legend = c("Ver. Err.", "Ver. SD"), col = c("black", "black"))



  if (printMaxErrors) {
    print(paste("Maximum Velocity Error:", round(max(err.vel.tot), digits = 1), "m/s. Predicted:", round(max(SD.vel.tot), digits = 1), "m/s"))

    print(paste("Maximum Verical Velocity Error:", round(max(err.vel.ver), digits = 1), "m/s. Predicted:", round(max(SD.vel.ver), digits = 1), "m/s"))
  }

  return(0)
}
