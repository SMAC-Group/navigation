#' @title Print trajectory Objects
#' @keywords internal
#' @method print trajectory
#' @description
#' Pretty formatting for \code{trajectory} objects.
#' @param x         A \code{trajectory} object.
#' @param obs       A \code{integer} the specifies how many from the beginning and end of the data set to show.
#' @param ...       Further arguments passed to or from other methods.
#' @return Print \code{trajectory} objects.
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#' @export
#' @examples
#' n <- 100
#' dat <- cbind(
#'   seq(from = 0, to = 60 * 60, length.out = n),
#'   46.204391 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   6.143158 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   375 + cumsum(rnorm(n))
#' )
#' traj <- make_trajectory(data = dat, name = "My cool data")
#' traj
print.trajectory <- function(x, obs = 5L, ...) {
  n <- nrow(x$trajectory)

  if (obs > n - 1 || obs < 1 || is.na(obs) || is.null(obs)) {
    stop("Error in parameter obs.")
  }

  if (!is.null(x$name)) {
    cat("Data Name:", x$name, "\n\n")
  }

  if (n > 2 * obs) {
    cat("Data preview:\n\n")
    print_lines <- as.matrix(rbind(head(x$trajectory, obs), tail(x$trajectory, obs)))
    print_lines <- rbind(head(print_lines, obs), "---" = "", tail(print_lines, obs))
    rownames(print_lines) <- format(rownames(print_lines), justify = "right")
    print.default(print_lines, right = TRUE, quote = FALSE)
    cat("\n")
  }
}
