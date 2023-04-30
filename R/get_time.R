#' @title Extract time for trajectory object
#' @description Extract time for trajectory object in seconds
#' @param x         A \code{trajectory} object.
#' @return A \code{numeric} vector with the time in seconds of a trajectory object.
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @examples
#' \dontrun{
#' n <- 1000
#' dat <- cbind(
#'   seq(from = 0, to = 50, length.out = n),
#'   46.204391 + cumsum(rnorm(n)) / 100,
#'   6.143158 + cumsum(rnorm(n)) / 100,
#'   375 + cumsum(rnorm(n))
#' )
#' traj <- make_trajectory(data = dat, name = "My cool data")
#' get_time(traj)
#' }
#' @noRd
get_time <- function(x) {
  if (inherits(x, "trajectory")) {
    x <- as.character(x$trajectory[, 1])
  } else if (inherits(x, "factor")) {
    x <- as.character(x)
  } else {
    stop("Invalid input class.")
  }
  n <- length(x)
  y <- rep(NA, n)

  for (i in 1:n) {
    inter <- strsplit(x[i], ":")[[1]]
    y[i] <- 60 * 60 * as.numeric(inter[1]) + 60 * as.numeric(inter[2]) + as.numeric(inter[3])
  }
  y
}
