#' @title Compute Coverage for the IMU states
#' @description works correctly only if there is only one process that is not a WN
#' @param sols The set of solutions returned by the \code{navigation} function
#' @param alpha size of the confidence interval
#' @param step Step.
#' @param idx Components to be considered (idx in [1, ..., 6])
#' @noRd
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
compute_coverage_imu_states <- function(sols, alpha = 0.95, step = 100, idx = 1:6) {
  if (max(idx) > 6) {
    stop("idx must be in [1, ..., 6]")
  }

  nsols <- length(sols$traj.fused)

  e <- compute_es_imu(sols, step, idx)

  # bounds = qchisq(c((1-alpha)/2,1-(1-alpha)/2), df=length(idx), lower.tail=TRUE)
  bounds <- qchisq(c(0, alpha), df = length(idx))

  out_b <- (e[2:(nsols + 1), ] > bounds[2]) + (e[2:(nsols + 1), ] < bounds[1])
  in_b <- -out_b + 1

  coverage <- matrix(0, nrow = 2, ncol = dim(e)[2])
  coverage[1, ] <- e[1, ]
  if (nsols > 1) {
    coverage[2, ] <- apply(in_b, 2, sum) / nsols
  } else {
    coverage[2, ] <- in_b
  }

  class(coverage) <- c("coverage.stat", "navigation.stat")
  attributes(coverage)$meta <- list(
    "type" = "Coverage IMU",
    "unit" = NA,
    "step" = step,
    "alpha" = alpha,
    "idx" = idx,
    "nruns" = nsols,
    "t0" = NULL,
    "tend" = NULL
  )

  return(coverage)
}
