#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Fnn <- function(X, imu_data, noise_info) {
  r <- X[7]
  p <- X[8]
  y <- X[9]

  # e.accgyr = cnstr.err(X_e, f_b_raw, omega_ib_b_raw)
  # a.acc = e.accgyr$e.acc
  # e.gyr = e.accgyr$e.gyr
  f_b_raw <- imu_data[2:4]
  e.acc <- cnstr.e_acc(X, noise_info)
  f_b <- f_b_raw - e.acc

  omega_ib_b_raw <- imu_data[5:7]
  e.gyr <- cnstr.e_gyr(X, noise_info)
  omega_ib_b <- omega_ib_b_raw - e.gyr

  Fnn <- matrix(0, nrow = 9, ncol = 9)

  # d/dr:
  Fnn[1, 4] <- Fnn[2, 5] <- Fnn[3, 6] <- 1

  # d/dV:
  Fnn[4:6, 7] <- rot.dC_b_i_dr(r, p, y) %*% f_b
  Fnn[4:6, 8] <- rot.dC_b_i_dp(r, p, y) %*% f_b
  Fnn[4:6, 9] <- rot.dC_b_i_dy(r, p, y) %*% f_b

  # d/drpy:
  Fnn[7:9, 7] <- rot.dCw_dr(r, p) %*% omega_ib_b
  Fnn[7:9, 8] <- rot.dCw_dp(r, p) %*% omega_ib_b

  return(Fnn)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Fee <- function(noise_info) {
  dn <- dim(noise_info)[1] - 6 # first 5 are white noises which don't count as states

  Fee <- matrix(0, nrow = dn, ncol = dn)

  if (dn > 0) {
    for (i in 1:dn) {
      i_n <- i + 6

      type <- noise_info[i_n, 1]
      if (type == 1) { # RW
        # nothing to do
      } else if (type == 2) { # DR
        # we need to search for the corresponding RW state
        # it is going to be some states before
        for (j in 1:i) {
          j_n <- j + 6
          # find the same sensor and the same axis but random walk
          if (noise_info[j_n, 1] == 1 && noise_info[j_n, 2] == noise_info[i_n, 2] && noise_info[j_n, 3] == noise_info[i_n, 3]) {
            Fee[j, i] <- 1
            break
          }
        }
      } else if (type == 3) { # GM
        Fee[i, i] <- -noise_info[i_n, 4] # -beta
      } else {
        stop("Error")
      }
    }
  }

  return(Fee)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Fne <- function(X, noise_info) {
  r <- X[7]
  p <- X[8]
  y <- X[9]
  C_b_i <- rot.C_b_i(r = r, p = p, y = y)
  Cw <- rot.Cw(r = r, p = p)

  dn <- dim(noise_info)[1] - 6 # first 6 are white noises which don't count as states

  Fne <- matrix(0, nrow = 9, ncol = dn)

  if (dn > 0) {
    for (i in 1:dn) {
      i_n <- i + 6

      type <- noise_info[i_n, 1]
      if (type == 1 || type == 3) { # RW or GM
        if (noise_info[i_n, 2] == 1) { # accelerometer
          Fne[4:6, i] <- -C_b_i[, noise_info[i_n, 3]] # according to sensor axis
        } else if (noise_info[i_n, 2] == 2) { # gyroscope
          Fne[7:9, i] <- -Cw[, noise_info[i_n, 3]] # according to sensor axis
        } else {
          # some mess
          stop("Error")
        }
      } else if (type == 2) { # DR
        # do nothing
      } else {
        stop("Error")
      }
    }
  }

  return(Fne)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
KF_mat.Fen <- function(noise_info) {
  dn <- dim(noise_info)[1] - 6 # first 5 are white noises which don't count as states
  Fen <- matrix(0, nrow = dn, ncol = 9)
  return(Fen)
}

#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
KF_mat.Fmat <- function(X, imu_data, noise_info, Fee) {
  Fnn <- KF_mat.Fnn(X, imu_data, noise_info)
  Fne <- KF_mat.Fne(X, noise_info)
  Fen <- KF_mat.Fen(noise_info)
  Fmat <- cbind(rbind(Fnn, Fen), rbind(Fne, Fee))
  return(Fmat)
}



#' @title TO DO
#' @description TO DO
#' @param TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
#'
KF_mat.Gmat <- function(X, noise_info) {
  r <- X[7]
  p <- X[8]
  y <- X[9]
  C_b_i <- rot.C_b_i(r = r, p = p, y = y)
  Cw <- rot.Cw(r = r, p = p)

  dn <- dim(noise_info)[1] - 6 # how many noise states

  j <- which(noise_info[, 1] != 2) # how many driving noises
  dd <- length(j)

  Gmat <- matrix(0, nrow = dn + 9, ncol = dd)

  for (i in 1:length(j)) {
    i_n <- j[i]
    if (noise_info[i_n, 1] == 0) { # white noise
      if (noise_info[i_n, 2] == 1) { # accelerometer
        Gmat[4:6, i] <- C_b_i[, noise_info[i_n, 3]] # according to the axis # TODO: verify, before it was -C_b_i
      } else {
        Gmat[7:9, i] <- Cw[, noise_info[i_n, 3]] # according to the axis # TODO: verify, before it was -Cw
      }
    } else {
      Gmat[9 + i_n - 6, i] <- 1
    }
  }

  return(Gmat)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Wmat <- function(noise_info) {
  j <- which(noise_info[, 1] != 2) # exclude drift
  Wmat <- diag(noise_info[j, 5]) # 5th col holds sigma2
  return(Wmat)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Hmat <- function(state_size) {
  Hmat.gps <- matrix(0, nrow = 6, ncol = state_size)
  Hmat.gps[1:6, 1:6] <- diag(1, 6, 6)

  Hmat.baro <- matrix(0, nrow = 1, ncol = state_size)
  Hmat.baro[3] <- -1

  Hmat <- list("gps" = Hmat.gps, "baro" = Hmat.baro)
  return(Hmat)
}



#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
KF_mat.Rmat <- function(mdl.gps = NULL, mdl.baro = NULL) {
  Rmat.gps <- matrix(0, nrow = 6, ncol = 6)
  Rmat.gps <- diag(c(
    mdl.gps$pos.hor$theta[mdl.gps$pos.hor$process.desc == "WN"],
    mdl.gps$pos.hor$theta[mdl.gps$pos.hor$process.desc == "WN"],
    mdl.gps$pos.ver$theta[mdl.gps$pos.ver$process.desc == "WN"],
    mdl.gps$vel.hor$theta[mdl.gps$vel.hor$process.desc == "WN"],
    mdl.gps$vel.hor$theta[mdl.gps$vel.hor$process.desc == "WN"],
    mdl.gps$vel.ver$theta[mdl.gps$vel.ver$process.desc == "WN"]
  ))

  Rmat.baro <- mdl.baro$theta[mdl.baro$process.desc == "WN"]

  Rmat <- list("gps" = Rmat.gps, "baro" = Rmat.baro)
  return(Rmat)
}
