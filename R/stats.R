get_it <- function(t, t0, tend, step) {
  if (is.null(t0)) {
    it0 <- 1
  } else {
    it0 <- which(t > t0 - 1e-6)[1]
  }
  if (is.null(tend)) {
    itend <- length(t)
  } else {
    itend <- max(which(t < tend + 1e-6))
  }

  it <- seq(it0, itend, step)

  return(it)
}



compute_es_imu <- function(sols, step = 50, idx = 1:6) {
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

  pb <- txtProgressBar(min = 1, max = nsols * length(it), style = 3)

  n <- 0
  for (i in 1:nsols) {
    for (j in 1:length(it)) { # exclude first minute
      ce <- sols$est.imu.states[[i]][idx, it[j], drop = FALSE] - sols$err.imu[[i]][idx + 1, it[j], drop = FALSE]
      P <- sols$Cov.Nav[[i]][idx + 9, idx + 9, it_p[j]]
      e[i + 1, j] <- t(ce) %*% solve(P) %*% ce

      n <- n + 1
      setTxtProgressBar(pb = pb, value = n)
    }
  }

  cat("\n\n")

  return(e)
}

compute_nees_and_coverage <- function(sols, alpha = 0.95, step = 100, idx = 1:6) {
  nsols <- length(sols$traj.fused)

  e <- compute_es(sols, step, idx)

  # bounds = qchisq(c((1-alpha)/2,1-(1-alpha)/2), df=length(idx), lower.tail=TRUE)
  bounds <- qchisq(c(0, alpha), df = length(idx))

  out_b <- (e[2:(nsols + 1), ] > bounds[2]) + (e[2:(nsols + 1), ] < bounds[1])
  in_b <- -out_b + 1

  coverage <- matrix(0, nrow = 2, ncol = dim(e)[2])
  coverage[1, ] <- e[1, ]

  nees <- matrix(0, ncol = dim(e)[2], nrow = 2)
  nees[1, ] <- e[1, ]

  if (dim(e)[1] > 2) {
    nees[2, ] <- apply(e[2:(nsols + 1), ], 2, mean)
    coverage[2, ] <- apply(in_b, 2, sum) / nsols
  } else {
    nees[2, ] <- e[2, ]
    coverage[2, ] <- in_b
  }
  return(list("nees" = nees, "coverage" = coverage))
}

sample_stat <- function(stats, t) {
  if (!inherits(stats, "list")) {
    stop("argument must be a list")
  }

  sstats <- array(NA, dim = c(dim(stats[[1]])[1], length(stats), length(t)))

  for (m in seq_along(stats)) {
    for (it in 1:length(t)) {
      i <- min(which(stats[[m]][1, ] > t[it] - 1e-6))

      sstats[, m, it] <- stats[[m]][, i]
    }
  }
  return(sstats)
}





# sample_covariance <- function(sols, idx = 1:3, step = 1, t0 = NULL, tend = NULL) {
#   nsols = length(sols$traj.fused)
#
#   it = get_it(sols$traj.fused[[1]]$trajectory[,1], t0, tend, step)
#
#   sigma2 = matrix(0, nrow = nsols*length(idx), ncol = length(it))
#
#   n = 0
#   for (i in 1:nsols) {
#     sigma2[(i-1)*length(idx)+1:length(idx),] = apply(res$Cov.Nav[[i]][idx,idx,it],3,diag)
#   }
#
#   sigma2_ret = matrix(0, nrow = length(idx)+1, ncol = length(it))
#   sigma2_ret[1,] = sols$traj.ref$trajectory[it,1]
#
#   for (i in 1:length(idx)) {
#     if (nsols > 1) {
#       sigma2_ret[i+1,] = apply(sigma2[(0:(nsols-1))*length(idx)+i,], 2, mean)
#     } else {
#       sigma2_ret[i+1,] = sigma2[i,]
#     }
#   }
#
#   return(sigma2_ret)
# }
