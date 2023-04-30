rot.C1 <- function(r) {
  r1 <- c(1, 0, 0)
  r2 <- c(0, cos(r), sin(r))
  r3 <- c(0, -sin(r), cos(r))
  C1 <- rbind(r1, r2, r3)
  return(C1)
}


rot.C2 <- function(p) {
  r1 <- c(cos(p), 0, -sin(p))
  r2 <- c(0, 1, 0)
  r3 <- c(sin(p), 0, cos(p))
  C2 <- rbind(r1, r2, r3)
  return(C2)
}


rot.C3 <- function(y) {
  r1 <- c(cos(y), sin(y), 0)
  r2 <- c(-sin(y), cos(y), 0)
  r3 <- c(0, 0, 1)
  C3 <- rbind(r1, r2, r3)
  return(C3)
}


rot.C_i_b <- function(r, p, y) { # X_b = C_i_b %*% X_i
  C_i_b <- rot.C1(r) %*% rot.C2(p) %*% rot.C3(y)
  return(C_i_b)
}


rot.C_b_i <- function(r, p, y) { # X_i = C_b_i %*% X_b
  C_b_i <- t(rot.C_i_b(r, p, y))
  return(C_b_i)
}


rot.dC1_dr <- function(r) {
  r1 <- c(0, 0, 0)
  r2 <- c(0, -sin(r), cos(r))
  r3 <- c(0, -cos(r), -sin(r))
  dC1 <- rbind(r1, r2, r3)
  return(dC1)
}


rot.dC2_dp <- function(p) {
  r1 <- c(-sin(p), 0, -cos(p))
  r2 <- c(0, 0, 0)
  r3 <- c(cos(p), 0, -sin(p))
  dC2 <- rbind(r1, r2, r3)
  return(dC2)
}


rot.dC3_dy <- function(y) {
  r1 <- c(-sin(y), cos(y), 0)
  r2 <- c(-cos(y), -sin(y), 0)
  r3 <- c(0, 0, 0)
  dC3 <- rbind(r1, r2, r3)
  return(dC3)
}


rot.dC_i_b_dr <- function(r, p, y) {
  dC_i_b_dr <- rot.dC1_dr(r) %*% rot.C2(p) %*% rot.C3(y)
  return(dC_i_b_dr)
}


rot.dC_i_b_dp <- function(r, p, y) {
  dC_i_b_dp <- rot.C1(r) %*% rot.dC2_dp(p) %*% rot.C3(y)
  return(dC_i_b_dp)
}


rot.dC_i_b_dy <- function(r, p, y) {
  dC_i_b_dy <- rot.C1(r) %*% rot.C2(p) %*% rot.dC3_dy(y)
  return(dC_i_b_dy)
}


rot.dC_b_i_dr <- function(r, p, y) {
  dC_b_i_dr <- t(rot.dC_i_b_dr(r, p, y))
  return(dC_b_i_dr)
}


rot.dC_b_i_dp <- function(r, p, y) {
  dC_b_i_dp <- t(rot.dC_i_b_dp(r, p, y))
  return(dC_b_i_dp)
}


rot.dC_b_i_dy <- function(r, p, y) {
  dC_b_i_dy <- t(rot.dC_i_b_dy(r, p, y))
  return(dC_b_i_dy)
}


rot.Cw <- function(r, p) {
  r1 <- c(1, tan(p) * sin(r), cos(r) * tan(p))
  r2 <- c(0, cos(r), -sin(r))
  r3 <- c(0, sin(r) / cos(p), cos(r) / cos(p))
  Cw <- rbind(r1, r2, r3)
  return(Cw)
}


rot.dCw_dr <- function(r, p) {
  r1 <- c(0, cos(r) * tan(p), -tan(p) * sin(r))
  r2 <- c(0, -sin(r), -cos(r))
  r3 <- c(0, cos(r) / cos(p), -sin(r) / cos(p))
  dCw_dr <- rbind(r1, r2, r3)
  return(dCw_dr)
}


rot.dCw_dp <- function(r, p) {
  r1 <- c(0, sin(r) / cos(p)^2, cos(r) / cos(p)^2)
  r2 <- c(0, 0, 0)
  r3 <- c(0, (sin(p) * sin(r)) / cos(p)^2, (cos(r) * sin(p)) / cos(p)^2)
  dCw_dr <- rbind(r1, r2, r3)
  return(dCw_dr)
}


rot.Cw_inv <- function(r, p) {
  r1 <- c(1, 0, -sin(p))
  r2 <- c(0, cos(r), cos(p) * sin(r))
  r3 <- c(0, -sin(r), cos(p) * cos(r))
  Cw_inv <- rbind(r1, r2, r3)
  return(Cw_inv)
}


rot.dCw_inv_dr <- function(r, p) {
  r1 <- c(0, 0, 0)
  r2 <- c(0, -sin(r), cos(p) * cos(r))
  r3 <- c(0, -cos(r), -cos(p) * sin(r))
  dCw_inv_dr <- rbind(r1, r2, r3)
  return(dCw_inv_dr)
}


rot.dCw_inv_dp <- function(r, p) {
  r1 <- c(0, 0, -cos(p))
  r2 <- c(0, 0, -sin(p) * sin(r))
  r3 <- c(0, 0, -cos(r) * sin(p))
  dCw_inv_dp <- rbind(r1, r2, r3)
  return(dCw_inv_dp)
}

rot.log <- function(C) {
  tr <- sum(diag(C))
  d <- 0.5 * (tr - 1.0)

  if (abs(d - 1.0) > 1.0e-6) {
    a <- acos(d) / (2.0 * sqrt(1.0 - d * d))
  } else {
    a <- (1202048.0 + d * (-757701.0 + 2.0 * d * (282368.0 + d * (-204183.0 + 4.0 * d * (31776.0 + d * (-15435.0 + 2.0 * d * (2656.0 + 7.0 * d * (-81.0 + 8.0 * d)))))))) / 1531530.0
  }

  wx <- a * (C - t(C))

  return(c(0.5 * (wx[3, 2] - wx[2, 3]), 0.5 * (wx[1, 3] - wx[3, 1]), 0.5 * (wx[2, 1] - wx[1, 2])))
}
