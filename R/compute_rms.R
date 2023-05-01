#' @title Compute Root Mean Squared error (RMS)
#' @description Compute Root Mean Squared error (RMS) for each solution
#' @param sols The set of solutions returned by the \code{navigation} function
#' @param step do it for one sample out of \code{step}
#' @param idx Components of the states to be considered (default: position)
#' @param t0 Start time for RMS calculation (default: beginning)
#' @param tend Start time for RMS calculation (default: end)
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @noRd
compute_rms <- function(sols, step = 1, idx = 1:3, t0 = NULL, tend = NULL) {
  if (max(idx) > 9) {
    stop("idx must be in [1, ..., 0]")
  }

  nsols <- length(sols$traj.fused)

  it <- get_it(sols$traj.fused[[1]]$trajectory[, 1], t0, tend, step)

  e <- matrix(0, nrow = nsols * length(idx), ncol = length(it))

  n <- 0
  for (i in 1:nsols) {
    e[(i - 1) * length(idx) + 1:length(idx), ] <- t((data.matrix(sols$traj.fused[[i]]$trajectory[it, idx + 1] - sols$traj.ref$trajectory[it, idx + 1]))^2)
  }

  rms <- matrix(0, nrow = length(idx) + 1, ncol = length(it))
  rms[1, ] <- sols$traj.ref$trajectory[it, 1]

  for (i in 1:length(idx)) {
    if (nsols > 1) {
      rms[i + 1, ] <- apply(e[(0:(nsols - 1)) * length(idx) + i, ], 2, mean)
    } else {
      rms[i + 1, ] <- e[i, ]
    }
  }

  rms[2:dim(rms)[1], ] <- sqrt(rms[2:dim(rms)[1], ])

  return(rms)
}
