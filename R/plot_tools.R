lims <- function(v) {
  c(min(v), max(v))
}



#' @title Plot Root Mean Squared error (RMS)
#' @description this function plots the RMS computed with the \code{compute_rms} function
#' @param rms a list of results obtained from \code{compute_rms}
#' @param idx Which components of the RMS to plot, default (1:3, position)
#' @param t0 start time (default: beginning)
#' @param tend stop time (defaut: end)
#' @param ylabels custom labels for the y axis (default: labels for position and orientation)
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
#' @noRd
plot_rms <- function(rms, idx = 1:3, t0 = NULL, tend = NULL, ylabels = c("N", "E", "D", "Roll", "Pitch", "Yaw")) {
  if (!inherits(rms, "list")) {
    stop("argument must be a list")
  }

  if (is.null(t0)) {
    it0 <- 1
  } else {
    it0 <- which(rms[[1]][1, ] > t0 - 1e-6)[1]
  }
  if (is.null(tend)) {
    itend <- length(rms[[1]][1, ])
  } else {
    itend <- max(which(rms[[1]][1, ] < tend + 1e-6))
  }

  ext <- array(NA, dim = c(dim(rms[[1]])[1] - 1, length(rms), 2))
  for (j in 1:length(rms)) {
    if (length(idx) > 1) {
      ext[, j, 1] <- apply(rms[[j]][idx + 1, it0:itend], 1, min)
      ext[, j, 2] <- apply(rms[[j]][idx + 1, it0:itend], 1, max)
    } else {
      ext[1, j, 1] <- min(rms[[j]][idx + 1, it0:itend])
      ext[1, j, 2] <- max(rms[[j]][idx + 1, it0:itend])
    }
  }

  # to define old par on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mfrow = c(length(idx), 1))

  for (i in idx) {
    plot(NA, ylim = c(min(ext[i, , 1]), max(ext[i, , 2])), xlim = c(rms[[1]][1, c(it0, itend)]), ylab = ylabels[i], xlab = "Time [s]")
    for (j in 1:length(rms)) {
      lines(rms[[j]][1, it0:itend], rms[[j]][i + 1, it0:itend], type = "l", col = j)
    }
  }

  par(mfrow = c(1, 1))
}

#' @importFrom graphics axis grid
#' @importFrom utils head
plot_sample_stat <- function(sstat, relative_to = NA, idx = 1:3, ylims, labels = c("E", "N", "D")) {
  
  # to define old par on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(length(idx), 1))

  for (i in idx) {
    plot(NA, xlim = c(min(sstat[1, 1, ]), max(sstat[1, 1, ])), ylim = ylims, xaxt = "n", ylab = paste(labels[i], "wrt correct [%]"), xlab = "at t [s]")
    axis(1, at = round(sstat[1, 1, ]))

    grid()
    for (im in 1:dim(sstat)[2]) {
      if (!is.na(relative_to)) {
        points(sstat[1, im, ], sstat[i + 1, im, ] / sstat[i + 1, relative_to, ] * 100, col = im, cex = 3, pch = 4)
      } else {
        points(sstat[1, im, ], sstat[i + 1, im, ], col = im, cex = 3, pch = 4)
      }
    }
  }

  par(mfrow = c(1, 1))
}

#'
#' @importFrom grDevices hcl
gg_color_hue <- function(n, alpha) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}


