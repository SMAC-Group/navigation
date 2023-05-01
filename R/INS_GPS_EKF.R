#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr mod
#' @importFrom RcppArmadillo armadillo_set_seed
#' @noRd
INS_GPS_EKF <- function(IC, imu_data, gps_data = NULL,
                        baro_data = NULL, KF.mdl = NULL, g,
                        noProgressBar = FALSE,
                        PhiQ_method = "exact", P_subsampling = 1,
                        compute_PhiQ_each_n = 10) {
  n <- dim(imu_data)[2]

  t <- imu_data[1, ]
  t_p <- t[seq(1, length(t), P_subsampling)]
  dt <- t[2] - t[1]

  # get noise info

  noise_info <- get_noise_info(KF.mdl$imu$error_model)
  state_names <- get_state_names(noise_info)

  error_size <- dim(noise_info)[1] - 6 # the first 6 are white noises
  state_size <- 9 + error_size

  # declaration:
  X_h <- matrix(0, nrow = state_size, ncol = n) # INS:9, Xe:6*(2+5) -> 6 single sensors * (2 for RW+DR and 5 for five GM1 processes)
  P_h <- array(0, dim = c(state_size, state_size, length(t_p))) # keep only one covariance out of P_subsampling
  P_curr <- matrix(0, nrow = state_size, ncol = state_size)


  # initialization:
  X_h[1:9, 1] <- IC$X0
  P_curr[1:9, 1:9] <- IC$P0 # current covariance

  # TODO: make better, some covariance for Drift states
  if (dim(noise_info)[1] > 6) {
    j <- which(noise_info[7:(dim(noise_info)[1]), 1] == 2)
    if (length(j) > 0) {
      P_curr[j + 9, j + 9] <- diag(noise_info[j + 6, 4])
    }
  }

  # KF fixed matrices
  Wmat <- KF_mat.Wmat(noise_info)
  Hmat <- KF_mat.Hmat(state_size)

  Rmat <- KF_mat.Rmat(mdl.gps = KF.mdl$gps$error_model, mdl.baro = KF.mdl$baro$error_model)

  Fee <- KF_mat.Fee(noise_info)

  t.eps <- 0.01 * dt

  if (!is.null(gps_data)) {
    k.gps <- 1
    dZgps <- matrix(0, nrow = 6, ncol = dim(gps_data)[2])
  }
  if (!is.null(baro_data)) {
    k.baro <- 1
    dZbaro <- matrix(0, nrow = 1, ncol = dim(baro_data)[2])
  }

  if (!noProgressBar) {
    pb <- txtProgressBar(min = 0, max = (n - 1), style = 3)
  }


  for (k in 1:n) {
    # update:
    if (!is.null(gps_data)) {
      if (k.gps <= dim(gps_data)[2]) {
        if (abs(gps_data[1, k.gps] - imu_data[1, k]) < t.eps) {
          N <- EKF.update(X_h[, k], P_curr, gps_data[2:7, k.gps], Hmat$gps, Rmat$gps)

          X_h[, k] <- N$X_kp
          P_curr <- N$P_kp
          dZgps[, k.gps] <- N$dZ

          k.gps <- k.gps + 1
          # cat("gps @ t = ", t[k+1], "\n")
        }
      }
    }

    if (!is.null(baro_data)) {
      if (k.baro <= dim(baro_data)[2]) {
        if (abs(baro_data[1, k.baro] - imu_data[1, k]) < t.eps) {
          N <- EKF.update(X_h[, k], P_curr, baro_data[2, k.baro], Hmat$baro, Rmat$baro)

          X_h[, k] <- N$X_kp
          P_curr <- N$P_kp
          dZbaro[, k.baro] <- N$dZ

          k.baro <- k.baro + 1
          # cat("baro @ t = ", t[k+1], "\n")
        }
      }
    }

    # store P only once every P_subsampling
    if (mod(k, P_subsampling) == 1) {
      icov <- floor(k / P_subsampling) + 1
      P_h[, , icov] <- P_curr
    }

    if (k < n) {
      # compute PhiQ
      if (k == 1 || k %% compute_PhiQ_each_n == 0) {
        # prediction:
        N <- EKF_pred_cpp(
          X_k = X_h[, k],
          P_k = P_curr,
          Fee = Fee,
          Wmat = Wmat,
          imu_data = imu_data[, k],
          noise_info = noise_info,
          g = g,
          dt = dt,
          method = toString(PhiQ_method)
        )

        # without cpp
        # N = EKF.pred(X_h[,k], P_curr, Fee, Wmat, imu_data[,k], noise_info, g, dt, PhiQ_method)

        X_h[, k + 1] <- N$X_kp
        P_curr <- N$P_kp

        last_Phi <- N$Phi
        last_Q <- N$Q

        # do not compute PhiQ and reuse previously estimated PhiQ
      } else {
        N <- EKF_pred_cpp_PhiQ_provided(
          X_k = X_h[, k],
          P_k = P_curr,
          Fee = Fee,
          Wmat = Wmat,
          imu_data = imu_data[, k],
          noise_info = noise_info,
          g = g,
          dt = dt,
          last_Phi = last_Phi,
          last_Q = last_Q
        )

        X_h[, k + 1] <- N$X_kp
        P_curr <- N$P_kp
      }
    }

    if (!noProgressBar) {
      setTxtProgressBar(pb = pb, value = k)
    }
  }

  if (!noProgressBar) {
    cat("\n\n")
  }

  if (is.null(gps_data) && is.null(baro_data)) {
    traj.calc <- list("state_names" = state_names, "X" = X_h, "P" = P_h, "t" = t, "t_p" = t_p)
  } else if (is.null(gps_data)) {
    traj.calc <- list("state_names" = state_names, "X" = X_h, "P" = P_h, "t" = t, "t_p" = t_p, "dZbaro" = dZbaro)
  } else if (is.null(baro_data)) {
    traj.calc <- list("state_names" = state_names, "X" = X_h, "P" = P_h, "t" = t, "t_p" = t_p, "dZgps" = dZgps)
  } else {
    traj.calc <- list("state_names" = state_names, "X" = X_h, "P" = P_h, "t" = t, "t_p" = t_p, "dZgps" = dZgps, "dZbaro" = dZbaro)
  }

  return(traj.calc)
}



#' @title EKF.pred
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
EKF.pred <- function(X_k, P_k, Fee, Wmat, imu_data, noise_info, g, dt, method) {
  X_kp <- c(
    pred.Xn(X_k = X_k, imu_data = imu_data, noise_info = noise_info, dt = dt, g = g),
    pred.Xe(X_k = X_k, noise_info = noise_info, dt = dt)
  )

  Fmat <- KF_mat.Fmat(X = X_k, imu_data = imu_data, noise_info = noise_info, Fee = Fee)
  Gmat <- KF_mat.Gmat(X = X_k, noise_info = noise_info)

  PhiQ <- pred_PhiQ_cpp_2(Fmat, Gmat, Wmat, dt, method = method)
  P_kp <- PhiQ$Phi %*% P_k %*% t(PhiQ$Phi) + PhiQ$Q

  return(list("X_kp" = X_kp, "P_kp" = P_kp))
}


#' @title EKF.pred
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
EKF.update <- function(X_k, P_k, z, H, R) {
  K <- P_k %*% t(H) %*% solve((H %*% P_k %*% t(H) + R))
  dZ <- z - H %*% X_k
  X_kp <- X_k + K %*% dZ
  P_kp <- (diag(1, length(X_k), length(X_k)) - K %*% H) %*% P_k

  return(list("X_kp" = X_kp, "P_kp" = P_kp, "dZ" = dZ))
}


#' @title EKF.pred
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
pred.Xn <- function(X_k, imu_data, noise_info, dt, g) {
  X_ned <- X_k[1:3]
  V_ned <- X_k[4:6]
  r <- X_k[7]
  p <- X_k[8]
  y <- X_k[9]

  f_b_raw <- imu_data[2:4]
  e_acc <- cnstr.e_acc(X_k, noise_info)
  f_b <- f_b_raw - e_acc

  omega_ib_b_raw <- imu_data[5:7]
  e_gyr <- cnstr.e_gyr(X_k, noise_info)
  omega_ib_b <- omega_ib_b_raw - e_gyr

  g_i <- c(0, 0, g)

  dX_ned <- V_ned
  dV_ned <- rot.C_b_i(r, p, y) %*% f_b + g_i
  drpy <- rot.Cw(r, p) %*% omega_ib_b

  dX <- c(dX_ned, dV_ned, drpy)

  Xn_kp1 <- X_k[1:9] + dX * dt

  return(Xn_kp1)
}



#' @title EKF.pred
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
pred.Xe <- function(X_k, noise_info, dt) {
  dn <- dim(noise_info)[1] - 6 # first 5 are white noises which don't count as states

  dX <- rep(0, dn)

  if (dn > 0) {
    for (i in 1:dn) {
      i_n <- i + 6

      type <- noise_info[i_n, 1]
      if (type == 0) {
        # some mess
        stop("Error")
      } else if (type == 1) { # RW
        # nothing to do, it dX dX[i] = 0
      } else if (type == 2) { # DR
        # we need to search for the corresponding RW state
        # it is going to be some states before
        for (j in 1:i) {
          j_n <- j + 6
          # find the same sensor and the same axis but random walk
          if (noise_info[j_n, 1] == 1 && noise_info[j_n, 2] == noise_info[i_n, 2] && noise_info[j_n, 3] == noise_info[i_n, 3]) {
            dX[j] <- dX[i]
            break
          }
        }
      } else if (type == 3) { # GM
        dX[i] <- (exp(-noise_info[i_n, 4] * dt) - 1) / dt * X_k[i + 9]
      }
    }

    Xe_kp1 <- X_k[10:(9 + dn)] + dX * dt
  } else {
    Xe_kp1 <- vector()
  }

  return(Xe_kp1)
}


#' @title TO DO
#' @description TO DO
#' @importFrom expm expm
#' @importFrom expm %^%
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
#' @importFrom expm %^%
pred.PhiQ <- function(Fmat, Gmat, Wmat, dt, method = "exact") {
  n <- dim(Fmat)[1]
  if (method == "exact") {
    A <- cbind(rbind(-Fmat, 0 * Fmat), rbind(Gmat %*% Wmat %*% t(Gmat), t(Fmat))) * dt
    B <- expm(A)
    B12 <- B[1:n, (n + 1):(2 * n)]
    B22 <- B[(n + 1):(2 * n), (n + 1):(2 * n)]
    Phi <- t(B22)
    Q <- Phi %*% B12
  } else if (method == 1) {
    Phi <- diag(1, n, n) + Fmat * dt
    q <- Gmat %*% Wmat %*% t(Gmat)
    Q <- q * dt
  } else if (method == 2) {
    Phi <- diag(1, n, n) + Fmat * dt + Fmat %*% Fmat * dt^2 / 2
    # Phi = diag(1,n,n) + Fmat*dt + Fmat%^%2*dt^2/2 original
    q <- Gmat %*% Wmat %*% t(Gmat)
    Q <- q * dt + (Fmat %*% q + q %*% t(Fmat)) * dt^2 / 2
  } else if (method == 3) {
    Phi <- diag(1, n, n) + Fmat * dt + Fmat %^% 2 * dt^2 / 2 + Fmat %^% 3 * dt^3 / 6
    q <- Gmat %*% Wmat %*% t(Gmat)
    Q <- q * dt + (Fmat %*% q + q %*% t(Fmat)) * dt^2 / 2 + (Fmat %*% q %*% t(Fmat) + 0.5 * Fmat %^% 2 %*% q + 0.5 * q %*% t(Fmat) %^% 2) * dt^3 / 3
  } else if (method == 4) {
    Phi <- diag(1, n, n) + Fmat * dt + Fmat %^% 2 * dt^2 / 2 + Fmat %^% 3 * dt^3 / 6 + Fmat %^% 4 * dt^4 / 24
    q <- Gmat %*% Wmat %*% t(Gmat)
    Q <- q * dt + (Fmat %*% q + q %*% t(Fmat)) * dt^2 / 2 + (Fmat %*% q %*% t(Fmat) + 0.5 * Fmat %^% 2 %*% q + 0.5 * q %*% t(Fmat) %^% 2) * dt^3 / 3 +
      (0.5 * Fmat %*% q %*% t(Fmat) %^% 2 + 0.5 * Fmat %^% 2 %*% q %*% t(Fmat) + 1 / 6 * Fmat %^% 3 %*% q + 1 / 6 * q %*% t(Fmat) %^% 3) * dt^4 / 4
  } else {
    stop("Invalid method.")
  }
  PhiQ <- list("Phi" = Phi, "Q" = Q)
  return(PhiQ)
}




#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
cnstr.e_acc <- function(X, noise_info) {
  # add together all the acc noises

  e_acc <- rep(0, 3)

  if (dim(noise_info)[1] > 6) {
    no_wn_noise_info <- noise_info[7:dim(noise_info)[1], ]

    for (ax in 1:3) {
      # select everything execpt DR for this axis
      #         | noise type            |   | sensor type           |   | sensor axis            |
      j <- which(no_wn_noise_info[, 1] != 2 & no_wn_noise_info[, 2] == 1 & no_wn_noise_info[, 3] == ax)
      e_acc[ax] <- sum(X[9 + j])
    }
  }

  return(e_acc)
}




#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
cnstr.e_gyr <- function(X, noise_info) {
  # add together all the gyro noises

  e_gyr <- rep(0, 3)

  if (dim(noise_info)[1] > 6) {
    no_wn_noise_info <- noise_info[7:dim(noise_info)[1], ]

    for (ax in 1:3) {
      # select everything execpt DR for this axis
      #         | noise type            |   | sensor type           |   | sensor axis            |
      j <- which(no_wn_noise_info[, 1] != 2 & no_wn_noise_info[, 2] == 2 & no_wn_noise_info[, 3] == ax)
      e_gyr[ax] <- sum(X[9 + j])
    }
  }

  return(e_gyr)
}


#' @title packs the needed parameters of noise models in one minimal size matrix
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
get_noise_info <- function(mdl.imu) {
  noise_info <- matrix(NA, nrow = 48, ncol = 5)
  colnames(noise_info) <- c("type", "sensor", "axis", "param_1", "param_2 (sigma2)")
  # type: 0 -> WN, 1 -> RW, 2 -> DR, 3 -> GM
  # sensor: 1 -> accelerometer, 2 -> gyroscope
  # axis: 1 -> x, 2 -> y, 3 -> z

  # imu errors
  for (j in seq_along(mdl.imu$acc)) { # for all axis
    m_jth_axis <- mdl.imu$acc[[j]]

    i <- 1
    n_gm <- 0
    while (i <= length(m_jth_axis$process.desc)) {
      if (m_jth_axis$process.desc[i] == "WN") {
        noise_info[j, ] <- c(0, 1, j, NA, m_jth_axis$theta[i])
      } else if (m_jth_axis$process.desc[i] == "RW") {
        noise_info[6 + j, ] <- c(1, 1, j, NA, m_jth_axis$theta[i])
      } else if (m_jth_axis$process.desc[i] == "DR") {
        noise_info[6 + j, ] <- c(1, 1, j, NA, 0) # put also random walk with sigma2 = 0
        noise_info[12 + j, ] <- c(2, 1, j, m_jth_axis$theta[i], NA)
      } else if (m_jth_axis$process.desc[i] == "BETA") {
        noise_info[18 + n_gm * 6 + j, ] <- c(3, 1, j, m_jth_axis$theta[i], m_jth_axis$theta[i + 1])
        n_gm <- n_gm + 1
        i <- i + 1
      } else {
        stop(paste("unsupported noise model", m_jth_axis$process.desc[i]))
      }
      i <- i + 1
    }
  }

  for (j in seq_along(mdl.imu$gyr)) { # for all axis
    m_jth_axis <- mdl.imu$gyr[[j]]

    i <- 1
    n_gm <- 0
    while (i <= length(m_jth_axis$process.desc)) {
      if (m_jth_axis$process.desc[i] == "WN") {
        noise_info[j + 3, ] <- c(0, 2, j, NA, m_jth_axis$theta[i])
      } else if (m_jth_axis$process.desc[i] == "RW") {
        noise_info[6 + 3 + j, ] <- c(1, 2, j, NA, m_jth_axis$theta[i])
      } else if (m_jth_axis$process.desc[i] == "DR") {
        noise_info[6 + 3 + j, ] <- c(1, 2, j, NA, 0) # put also random walk with sigma2 = 0
        noise_info[12 + 3 + j, ] <- c(2, 2, j, m_jth_axis$theta[i], NA)
      } else if (m_jth_axis$process.desc[i] == "BETA") {
        noise_info[18 + 3 + n_gm * 6 + j, ] <- c(3, 2, j, m_jth_axis$theta[i], m_jth_axis$theta[i + 1])
        n_gm <- n_gm + 1
        i <- i + 1
      } else {
        stop(paste("unsupported noise model", m_jth_axis$process.desc[i]))
      }
      i <- i + 1
    }
  }

  noise_info <- noise_info[!is.na(noise_info[, 1]), ]

  return(noise_info)
}

#' @title builds the state vector
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
get_state_names <- function(noise_info) {
  state_names <- c("p_x", "p_y", "p_z", "v_x", "v_y", "v_z", "roll", "pitch", "yaw")

  # TODO put a number for the GM
  if (dim(noise_info)[1] > 6) {
    for (i in 7:dim(noise_info)[1]) {
      state_names <- c(state_names, paste(
        switch(noise_info[i, 2],
          "acc",
          "gyro"
        ),
        switch(noise_info[i, 3],
          "x",
          "y",
          "z"
        ),
        switch(noise_info[i, 1] + 1,
          "wn",
          "rw",
          "dr",
          "gm"
        ),
        sep = "_"
      ))
    }
  }

  return(state_names)
}
