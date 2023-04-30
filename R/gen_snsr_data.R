#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#' @noRd
#' @importFrom simts gen_gts
#' @importFrom stats approx
gen_snsr_data <- function(snsr.name, traj, timing, snsr.mdl = NULL, g = NULL, clean_data = FALSE) {
  t <- traj$trajectory$time
  t1 <- t[1]
  t2 <- tail(t, n = 1)

  if (!xor("error_model" %in% names(snsr.mdl), "error_data" %in% names(snsr.mdl))) {
    stop("snsr.mdl must contain either error_mode or error_data")
  }

  if (snsr.name == "baro") {
    # Generate clean snsr.data----
    snsr.t <- seq(from = t1, to = t2, by = 1 / timing$freq.baro)
    snsr.data <- matrix(NA, nrow = 2, ncol = length(snsr.t))
    snsr.data[1, ] <- snsr.t
    alt <- -traj$trajectory$x_D
    snsr.data[2, ] <- approx(x = t, y = alt, xout = snsr.t)$y

    # Contaminate snsr.data----
    if (!clean_data) {
      if ("error_model" %in% names(snsr.mdl)) {
        N <- dim(snsr.data)[2]
        alt_err <- gen_gts(n = N, model = snsr.mdl$error_model, freq = timing$freq.baro)
        snsr.data[2, ] <- snsr.data[2, ] + alt_err

        snsr.err <- matrix(NA, nrow = 2, ncol = length(snsr.t))
        snsr.err[1, ] <- snsr.t
        snsr.err[2, ] <- alt_err
      } else if ("error_data" %in% names(snsr.mdl)) {
        stop("TODO: baro noise from error_data")
      }
    } else {
      snsr.err <- NA
    }
  } else if (snsr.name == "gps") {
    # Generate clean snsr.data----
    snsr.t <- seq(from = t1, to = t2, by = 1 / timing$freq.gps)

    # remove sample during gps outage
    i_in <- rep(TRUE, length(snsr.t))
    for (j in 1:length(timing$gps.out.start)) {
      i_in[snsr.t > timing$gps.out.start[j] - 1e-6 & snsr.t < timing$gps.out.end[j] + 1e-6] <- FALSE
    }

    # snsr.t = snsr.t[snsr.t<=timing$gps.out.start | snsr.t>timing$gps.out.end]
    snsr.t <- snsr.t[i_in]
    snsr.data <- matrix(NA, nrow = 7, ncol = length(snsr.t))
    snsr.data[1, ] <- snsr.t

    # XV_NED     = traj[c(2:4,5:7),] # I know... I had a point to make! ;)
    XV_NED <- t(cbind(
      traj$trajectory$x_N,
      traj$trajectory$x_E,
      traj$trajectory$x_D,
      traj$trajectory$v_N,
      traj$trajectory$v_E,
      traj$trajectory$v_D
    ))

    for (i in 2:7) {
      snsr.data[i, ] <- approx(x = t, y = XV_NED[i - 1, ], xout = snsr.t)$y
    }

    # Contaminate snsr.data----
    if (!clean_data) {
      if ("error_model" %in% names(snsr.mdl)) {
        N <- dim(snsr.data)[2]

        pos_err_hor_1 <- gen_gts(n = N, model = snsr.mdl$error_model$pos.hor, freq = timing$freq.gps)
        pos_err_hor_2 <- gen_gts(n = N, model = snsr.mdl$error_model$pos.hor, freq = timing$freq.gps)
        pos_err_ver <- gen_gts(n = N, model = snsr.mdl$error_model$pos.ver, freq = timing$freq.gps)

        vel_err_hor_1 <- gen_gts(n = N, model = snsr.mdl$error_model$vel.hor, freq = timing$freq.gps)
        vel_err_hor_2 <- gen_gts(n = N, model = snsr.mdl$error_model$vel.hor, freq = timing$freq.gps)
        vel_err_ver <- gen_gts(n = N, model = snsr.mdl$error_model$vel.ver, freq = timing$freq.gps)


        snsr.data <- rbind(
          snsr.data[1, ],
          snsr.data[2:4, ] + t(cbind(pos_err_hor_1, pos_err_hor_2, pos_err_ver)),
          snsr.data[5:7, ] + t(cbind(vel_err_hor_1, vel_err_hor_2, vel_err_ver))
        )

        snsr.err <- matrix(NA, nrow = 7, ncol = length(snsr.t))
        snsr.err <- rbind(
          snsr.data[1, ],
          t(cbind(pos_err_hor_1, pos_err_hor_2, pos_err_ver)),
          t(cbind(vel_err_hor_1, vel_err_hor_2, vel_err_ver))
        )
      } else if ("error_data" %in% names(snsr.mdl)) {
        stop("TODO: gps noise from error_data")
      }
    } else {
      snsr.err <- NA
    }
  } else if (snsr.name == "imu") {
    # Generate clean snsr.data----
    N <- dim(traj$trajectory)[1]
    t <- traj$trajectory$time
    dt <- (tail(t, 1) - t[1]) / (N - 1) # traj must be regularized first (see check_traj.R)

    V_NED <- t(cbind(
      traj$trajectory$v_N,
      traj$trajectory$v_E,
      traj$trajectory$v_D
    ))
    rpy <- t(cbind(
      traj$trajectory$roll,
      traj$trajectory$pitch,
      traj$trajectory$yaw
    ))

    g_i <- c(0, 0, g)

    # f_b
    f_b <- matrix(NA, nrow = 3, ncol = N)
    for (k in 1:(N - 1)) {
      C_b_i_k <- rot.C_b_i(r = rpy[1, k], p = rpy[2, k], y = rpy[3, k])
      f_b[, k] <- t(C_b_i_k) %*% ((V_NED[, k + 1] - V_NED[, k]) / dt - g_i)
    }
    f_b[, N] <- f_b[, N - 1]

    # Omega_ib_b
    Omega_ib_b <- matrix(NA, nrow = 3, ncol = N)
    for (k in 1:(N - 1)) {
      C_w_inv_k <- rot.Cw_inv(r = rpy[1, k], p = rpy[2, k])
      Omega_ib_b[, k] <- C_w_inv_k %*% (rpy[, k + 1] - rpy[, k]) / dt
    }
    Omega_ib_b[, N] <- Omega_ib_b[, N - 1]

    snsr.data <- rbind(t, f_b, Omega_ib_b)


    # Contaminate snsr.data----
    if (!clean_data) {
      N <- dim(snsr.data)[2]

      if ("error_model" %in% names(snsr.mdl)) {
        acc_err_1 <- gen_gts(n = N, model = snsr.mdl$error_model$acc$X, freq = timing$freq.imu)
        acc_err_2 <- gen_gts(n = N, model = snsr.mdl$error_model$acc$Y, freq = timing$freq.imu)
        acc_err_3 <- gen_gts(n = N, model = snsr.mdl$error_model$acc$Z, freq = timing$freq.imu)

        gyr_err_1 <- gen_gts(n = N, model = snsr.mdl$error_model$gyr$X, freq = timing$freq.imu)
        gyr_err_2 <- gen_gts(n = N, model = snsr.mdl$error_model$gyr$Y, freq = timing$freq.imu)
        gyr_err_3 <- gen_gts(n = N, model = snsr.mdl$error_model$gyr$Z, freq = timing$freq.imu)
      } else if ("error_data" %in% names(snsr.mdl)) {
        if (!inherits(snsr.mdl$error_data, "imu")) {
          stop("snsr.mdl$error_data must inherit from imu")
        }

        if (timing$freq.imu != attr(snsr.mdl$error_data, "freq")) {
          warning(paste("snsr.mdl$error_data has a different sampling frequency wrt timing: ", timing$freq.imu, "vs", attr(snsr.mdl$error_data, "freq")))
        }

        if (N > dim(snsr.mdl$error_data)[1]) {
          stop("not enough samples in snsr.mdl$error_data")
        }

        means <- apply(snsr.mdl$error_data, 2, mean)

        iax <- match("Accel. X", attr(snsr.mdl$error_data, "dimnames")[[2]])
        iay <- match("Accel. Y", attr(snsr.mdl$error_data, "dimnames")[[2]])
        iaz <- match("Accel. Z", attr(snsr.mdl$error_data, "dimnames")[[2]])

        igx <- match("Gyro. X", attr(snsr.mdl$error_data, "dimnames")[[2]])
        igy <- match("Gyro. Y", attr(snsr.mdl$error_data, "dimnames")[[2]])
        igz <- match("Gyro. Z", attr(snsr.mdl$error_data, "dimnames")[[2]])


        i0 <- runif(1, 1, dim(snsr.mdl$error_data)[1] - N + 1)

        acc_err_1 <- snsr.mdl$error_data[i0:(i0 + N - 1), iax] - means[iax]
        acc_err_2 <- snsr.mdl$error_data[i0:(i0 + N - 1), iay] - means[iay]
        acc_err_3 <- snsr.mdl$error_data[i0:(i0 + N - 1), iaz] - means[iaz]

        gyr_err_1 <- snsr.mdl$error_data[i0:(i0 + N - 1), igx] - means[igx]
        gyr_err_2 <- snsr.mdl$error_data[i0:(i0 + N - 1), igy] - means[igy]
        gyr_err_3 <- snsr.mdl$error_data[i0:(i0 + N - 1), igz] - means[igz]
      }

      snsr.data <- rbind(
        snsr.data[1, ],
        snsr.data[2:4, ] + t(cbind(acc_err_1, acc_err_2, acc_err_3)),
        snsr.data[5:7, ] + t(cbind(gyr_err_1, gyr_err_2, gyr_err_3))
      )

      snsr.err <- rbind(
        snsr.data[1, ],
        t(cbind(acc_err_1, acc_err_2, acc_err_3)),
        t(cbind(gyr_err_1, gyr_err_2, gyr_err_3))
      )
    } else {
      snsr.err <- NA
    }
  }

  # Output snsr.data and timing (might have been modified)----
  out <- list("snsr.data" = snsr.data, "snsr.err" = snsr.err, "timing" = timing)
  return(out)
}
