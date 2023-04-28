lims <- function(v) {
  c(min(v), max(v))
}

#' @title Plot navigation states with covariance
#' @description this function plots the navigation states with estimated covariance of a solution computed with the \code{navigation} function
#' @param sol The set of solutions returned by the \code{navigation} function
#' @param idx Which Monte-Carlo solution to plot (can be a vector)
#' @param cov_idx Which Monte-Carlo solution to use for confidence intervals
#' @param error Wether to plot the error with respect to the refefence or the estimated values
#' @param step Plot one time out of \code{step}
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
plot_nav_states_with_cov <- function(sol, idx=1, cov_idx=1, error = TRUE, step = 10) {
  if (length(cov_idx) > 1) {
    stop("cov_idx must be of length 1")
  }
  
  titles=colnames(sol$traj.fused[[ idx[1] ]]$trajectory)[2:7]

  t = sol$t
  t_p = sol$t_p
  
  it = seq(1, length(t), step)

  cols = gg_color_hue(2, 1)
  
  par(mfrow=c(3,1))

  for (j in 0:1) {
    if (j == 0) {
      scale = 1
    } else {
      scale = 180/pi
    }
    
    for (i in 1:3) {
      if (error) {
        v1 = rep(0,length(it))
      } else {
        v1 = sol$traj.ref$trajectory[it,1+3*j+i] * scale
      }
      
      v2 = list()
      for (k in 1:length(idx)) {
        if (error) {
          v2[[k]] = (sol$traj.ref$trajectory[it,1+3*j+i] - sol$traj.fused[[idx[k]]]$trajectory[it,1+3*j+i]) * scale
        } else {
          v2[[k]] = sol$traj.fused[[idx[k]]]$trajectory[it,1+3*j+i] * scale
        }
      }

      dcov = apply(sol$Cov.Nav[[ cov_idx ]][,,], 3, diag)
      dstd = 3*sqrt(dcov[3*j+i,]) * scale
      dstd_int = approx(t_p, dstd, t[it])$y
      
      v3 = dstd_int+v1
      v4 = -dstd_int+v1

      l1 = apply(rbind(v1, do.call(rbind, v2), v3, v4), 1, lims)
      l2 = c(min(l1[1,]), max(l1[2,]))

      plot(NA, xlab = "time [s]", ylab = titles[3*j+i], ylim = l2, xlim = c(t[it[1]], t[tail(it,1)]) )
      
      polygon(c(t[it], rev(t[it])), c(v3,rev(v4)), col = "#acacac60", border = NA)
      
      for (k in seq_along(v2)) {
        lines(t[it], v2[[k]], type = "l", col = cols[1])
      }
      
      lines(t[it], v1, type = "l", col = "black", lw=1)
    }
  }

  par(mfrow=c(1,1))
}

#' @title Plot IMU error with covariances
#' @description this function plots the estimated IMU errors with covariance of a solution computed with the \code{navigation} function
#' @param sol The set of solutions returned by the \code{navigation} function
#' @param idx Which Monte-Carlo solution to plot
#' @param error Whether to plot the error with respect to the reference or the estimated values
#' @param step Plot one time out of \code{step}
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
plot_imu_err_with_cov <- function(sol, idx=1, error = TRUE, step=10) {
  if (dim(sol$Cov.Nav[[idx]])[1] > 9 ) {
    titles=c("Acc_x", "Acc_y", "Acc_z", "Gyro_x", "Gyro_y", "Gyro_z")
    
    search_names = c("acc_x", "acc_y", "acc_z", "gyro_x", "gyro_y", "gyro_z")

    t = sol$err.imu[[idx]][1,]
    t_p = sol$t_p
    
    it = seq(1, length(t), step)

    cols = gg_color_hue(2, 1)

    par(mfrow=c(3,1))

    for (j in 0:1) {
      for (i in 1:3) {
        
        k = which(startsWith(sol$state_names, search_names[i+3*j]))

        if (error) {
          v1 = rep(0,length(it))
          v2 = sol$err.imu[[idx]][1+i+3*j,it] - colSums(sol$est.imu.states[[idx]][k-9,it,drop=FALSE])
        } else {
          v1 = sol$err.imu[[idx]][1+i+3*j,it]
          v2 = colSums(sol$est.imu.states[[idx]][k-9,it,drop=FALSE])
        }

        dcov = apply(sol$Cov.Nav[[idx]][,,], 3, diag)
        dstd = 3*sqrt(colSums(dcov[k,,drop=FALSE]))
        dstd_int = approx(t_p, dstd, t[it])$y
        
        v3 = dstd_int+v1
        v4 = -dstd_int+v1

        l1 = apply(rbind(v1,v2,v3,v4), 1, lims)
        l2 = c(min(l1[1,]), max(l1[2,]))

        plot(NA, xlab = "time [s]", ylab = titles[i+3*j], ylim = l2, xlim = c(t[it[1]], t[tail(it,1)]) )
        
        polygon(c(t[it], rev(t[it])), c(v3,rev(v4)), col = "#acacac60", border = NA)
        
        lines(t[it], v2, type = "l", col = cols[1])
        
        lines(t[it], v1, type = "l", col = "black", lw=1)
      }
    }
  } else {
    warning("no error states are estimated from the filter.")
  }
}

#' @title Plot Root Mean Squared error (RMS)
#' @description this function plots the RMS computed with the \code{compute_rms} function
#' @param rms a list of results obtained from \code{compute_rms}
#' @param idx Which components of the RMS to plot, default (1:3, position)
#' @param t0 start time (default: beginning)
#' @param tend stop time (defaut: end)
#' @param ylabels custom labels for the y axis (default: labels for position and orientation)
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'

plot_rms <- function(rms, idx = 1:3, t0 = NULL, tend = NULL, ylabels = c("N", "E", "D", "Roll", "Pitch", "Yaw")) {
  if (!inherits(rms, "list") ) {
    stop("argument must be a list")
  }

  if (is.null(t0)) {
    it0 = 1
  } else {
    it0 = which(rms[[1]][1,] > t0-1e-6)[1]
  }
  if (is.null(tend)) {
    itend = length( rms[[1]][1,] )
  } else {
    itend = max(which(rms[[1]][1,] < tend+1e-6))
  }
  
  ext = array(NA, dim = c(dim(rms[[1]])[1]-1, length(rms), 2))
  for (j in 1:length(rms)) {
    if (length(idx)>1) {
      ext[,j,1] = apply(rms[[j]][idx+1,it0:itend], 1, min)
      ext[,j,2] = apply(rms[[j]][idx+1,it0:itend], 1, max)
    } else {
      ext[1,j,1] = min(rms[[j]][idx+1,it0:itend])
      ext[1,j,2] = max(rms[[j]][idx+1,it0:itend])
    }
  }
  
  
  par(mfrow=c(length(idx),1))
  
  for (i in idx) {
    plot(NA, ylim=c( min(ext[i,,1]), max(ext[i,,2]) ), xlim=c(rms[[1]][1,c(it0, itend)]), ylab=ylabels[i], xlab="Time [s]")
    for (j in 1:length(rms)) {
      lines(rms[[j]][1,it0:itend], rms[[j]][i+1,it0:itend], type = "l", col=j)
    }
  }
  
  par(mfrow=c(1,1))
}

#' @importFrom graphics axis grid 
#' @importFrom utils head
plot_sample_stat = function(sstat, relative_to = NA, idx = 1:3, ylims, labels = c("E", "N", "D")) {
  
  par(mfrow=c(length(idx),1))
  
  for (i in idx) {
    plot(NA, xlim = c(min(sstat[1,1,]), max(sstat[1,1,])), ylim = ylims, xaxt = "n", ylab=paste(labels[i],"wrt correct [%]"), xlab="at t [s]")
    axis(1, at = round(sstat[1,1,]))
    
    grid()
    for (im in 1:dim(sstat)[2]) {
      if (!is.na(relative_to)) {
        points(sstat[1,im,], sstat[i+1,im,]/sstat[i+1,relative_to,]*100, col = im, cex = 3, pch = 4)
      } else {
        points(sstat[1,im,], sstat[i+1,im,], col = im, cex = 3, pch = 4)
      }
    }
  }
  
  par(mfrow=c(1,1))
  
}

#'
#' @importFrom grDevices hcl
gg_color_hue <- function(n, alpha) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

#' @title Plot multiple \code{navigation.stat} objects
#' @description plot multiple stats alltogether
#' @param ... navigation statistics, e.g., computed with \code{compute_mean_position_err}
#' @export
#' @param legend The legend
#' @param title The title
#' @param xlim xlim
#' @param ylim ylim
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
#'
plot.navigation.stat <- function(..., legend=NA, title=NA, xlim=c(NA, NA), ylim=c(NA, NA)) {
  
  stats = list(...)
  
  # check arguments
  for (i in seq_along(stats)) {
    if (!inherits(stats[[i]], "navigation.stat")) {
      stop(paste("argument",i,"is not a navigation.stat"))
    }
  }
  
  # compute axis limits
  
  Lx = matrix(0, nrow = length(stats), ncol = 2)
  Ly = matrix(0, nrow = length(stats), ncol = 2)
  for (i in seq_along(stats)) {
    Lx[i,] = lims(stats[[i]][1,])
    Ly[i,] = lims(stats[[i]][2,])
  }
  Lx = ifelse(is.na(xlim), c(min(Lx[,1]), max(Lx[,2])), xlim)
  Ly = ifelse(is.na(ylim), c(min(Ly[,1]), max(Ly[,2])), ylim)

  # plot
  
  plot(NA, 
       xlim = Lx, 
       ylim = Ly, 
       xlab = "Time [s]", 
       ylab = attributes(stats[[1]])$meta$unit, 
       main = 
         if (is.na(title)) {
            attributes(stats[[1]])$meta$type
         } else {
           title
         }
  )
           
  cols = gg_color_hue(length(stats), 1)
  
  for (i in seq_along(stats)) {
    lines(stats[[i]][1,], stats[[i]][2,], type = "l", col = cols[i])
  }
  
  if (! is.na(legend[[1]]) ) {
    legend("top", col=cols, lt=1, legend=legend)
  }
}

#' @title Plot multiple \code{nees.stat} objects
#' @description plot multiple \code{nees.stat} objects alltogether
#' @param ... NEES, e.g., computed with \code{compute_nees}
#' @param alpha for the confidence interval plot
#' @param legend legend of the plot.
#' @param title title of the plot.
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
plot.nees.stat = function(..., alpha = 0.95, legend=NA, title=NA) {
  stats = list(...)
  
  # check arguments
  for (i in seq_along(stats)) {
    if (!inherits(stats[[i]], "nees.stat")) {
      stop(paste("argument",i,"is not a nees.stat"))
    }
  }
  
  do.call(plot.navigation.stat, c(stats, list( "legend" = legend, "title" = title)))
  
  # plot bounds
  
  dof = length(attributes(stats[[1]])$meta$idx)
  nruns = attributes(stats[[1]])$meta$nruns
  
  bounds = qchisq(c( (1-alpha)/2, 1-(1-alpha)/2 ), df=dof*nruns, lower.tail=TRUE)/nruns
  #bounds = qgamma(c( (1-alpha)/2, 1-(1-alpha)/2 ), shape = nruns*dof/2, scale = 2/nruns)
  
  abline(h = bounds[1], lt=2, col="black")
  abline(h = bounds[2], lt=2, col="black")
}

#' @title Plot multiple \code{coverage.stat} objects
#' @description plot multiple coverages alltogether
#' @param ... coverage, e.g., computed with \code{compute_coverage}
#' @param legend Legend of the plot.
#' @param title Title of the plot.
#' @export
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
plot.coverage.stat = function(..., legend=NA, title=NA) {
  stats = list(...)
  
  # check arguments
  for (i in seq_along(stats)) {
    if (!inherits(stats[[i]], "coverage.stat")) {
      stop(paste("argument",i,"is not a coverage.stat"))
    }
  }
  
  do.call(plot.navigation.stat, c(stats, list( "legend" = legend, "title" = title, "ylim"=c(0,1))))
  
  # plot bounds
  
  alpha = attributes(stats[[1]])$meta$alpha
  nruns = attributes(stats[[1]])$meta$nruns

  bounds = alpha + c(-1, 1)*qnorm(1 - (1-alpha)/2)*sqrt(alpha*(1-alpha)/nruns)
  
  abline(h = bounds, lt=2, col="black")
}
