compute_es <- function(sols, step = 50, idx = 1:6, progressbar = FALSE) {
  nsols <- length(sols$traj.fused)

  t <- sols$t
  t_p <- sols$t_p

  # check that covariance exists where asked
  if (step < sols$Cov_subsampling || round(step / sols$Cov_subsampling) != step / sols$Cov_subsampling) {
    stop("Covariance is not available at the required step")
  }

  it <- seq(step + 1, length(t), step)
  it_p <- match(t[it], t_p)

  e <- matrix(0, nrow = nsols + 1, ncol = length(it))
  e[1, ] <- t[it]

  if (progressbar) {
    pb <- txtProgressBar(min = 1, max = nsols * length(it), style = 3)
  }

  n <- 0
  for (i in 1:nsols) {
    for (j in 1:length(it)) { # exclude first minute
      ce <- data.matrix(sols$traj.fused[[i]]$trajectory[it[j], idx + 1] - sols$traj.ref$trajectory[it[j], idx + 1])
      P <- sols$Cov.Nav[[i]][idx, idx, it_p[j]]
      e[i + 1, j] <- ce %*% solve(P) %*% t(ce)

      n <- n + 1
      if (progressbar) {
        setTxtProgressBar(pb = pb, value = n)
      }
    }
  }

  cat("\n\n")

  return(e)
}
