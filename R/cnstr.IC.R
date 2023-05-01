#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @noRd
cnstr.IC <- function(traj.ref, KF.mdl) {
  # X0[1:9] = traj.ref[2:10,1]
  # X0[1:9] = traj.ref$trajectory[colnames(traj$trajectory)==c("x_N","x_E","x_D","v_N","v_E","v_D","roll","pitch","yaw"), 1]
  X0 <- c(
    traj.ref$trajectory$x_N[1],
    traj.ref$trajectory$x_E[1],
    traj.ref$trajectory$x_D[1],
    traj.ref$trajectory$v_N[1],
    traj.ref$trajectory$v_E[1],
    traj.ref$trajectory$v_D[1],
    traj.ref$trajectory$roll[1],
    traj.ref$trajectory$pitch[1],
    traj.ref$trajectory$yaw[1]
  )

  P0 <- matrix(0, nrow = 9, ncol = 9)

  IC <- list("X0" = X0, "P0" = P0)
  return(IC)
}
