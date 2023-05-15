#' @title Plot a \code{trajectory} object
#' @description Plot a \code{trajectory} object in 2D or 3D.
#' @method plot trajectory
#' @export
#' @param x                 A \code{trajectory} object
#' @param threeD            A \code{boolean} indicating whether the plot should be 3D or 2D (default \code{FALSE}, 2D).
#' @param col               A \code{string} corresponding to the color of the line used for 2D trajectory (default \code{"blue4"}).
#' @param col_start         A \code{string} corresponding to the color of the point used to denote the beginning of a 2D trajectory (default \code{"green3"}).
#' @param col_end           A \code{string} corresponding to the color of the point used to denote the end of a 2D trajectory (default \code{"red2"}).
#' @param add_altitude      A \code{boolean} to indicate if the altitude should be plotted in 2D trajectory in NED system (default \code{TRUE}; altitude is plotted).
#' @param pch_points_start  A \code{numeric} corresponding to the symbol (pch) of the points used to denote the beginning of a 2D trajectory (default \code{15}).
#' @param pch_points_end    A \code{numeric} corresponding to the symbol (pch) of the points used to denote the end of a 2D trajectory (default \code{16}).
#' @param cex_points        A \code{numeric} corresponding to the size (cex) of the points used to denote the beginning and the end of a 2D trajectory (default \code{1.5}).
#' @param n_split           A \code{numeric} for the number of ticks in 2D plot with altitude profile, if NULL no ticks are added (default = \code{6}).
#' @param plot_end_points   A \code{boolean} to indicate if points should be plotted at the beginning and the end of a 2D trajectory (default \code{TRUE}; points are plotted).
#' @param add_title         A \code{boolean} or \code{string}. If a \code{boolean} is used it indicates if a title should be added to 2D trajectory (only active if name of trajectory exist); if a \code{string} is used it corresponds to the title (default \code{TRUE}).
#' @param threeD_line_width A \code{numeric} corresponding to the width of the line for a 3D trajectory (default \code{4}).
#' @param threeD_line_color A \code{string} corresponding to the hex color code of the line used for a 3D trajectory (default \code{"#008080"}).
#' @param threeD_col_grad   A \code{boolean} to indicate if a color gradient should be used for a 3D trajectory (default \code{FALSE}).
#' @param threeD_grad_start A \code{string} corresponding to the hex color code for the start of the gradient (default \code{"#008080"}).
#' @param threeD_grad_end   A \code{string} corresponding to the hex color code for the end of the gradient (default \code{"#ab53cf"}).
#' @param ...               Additional arguments affecting the plot produced.
#' @return A trajectory plot.
#' @importFrom plotly plot_ly
#' @importFrom leaflet leaflet addTiles addMarkers addPolylines
#' @importFrom magrittr "%>%"
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#' @examples
#' n <- 100
#' set.seed(123)
#' dat <- cbind(
#'   seq(from = 0, to = 60 * 60, length.out = n),
#'   46.204391 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   6.143158 * pi / 180 + cumsum(rnorm(n)) / 10^5,
#'   375 + cumsum(rnorm(n))
#' )
#' traj <- make_trajectory(data = dat, name = "My cool data")
#' plot(traj)
#' plot(traj, threeD = TRUE)
#' plot(traj,
#'   threeD = TRUE, threeD_line_width = 8,
#'   threeD_line_color = "#e74c3c"
#' )
#' plot(traj,
#'   threeD = TRUE,
#'   threeD_col_grad = TRUE
#' )
#' plot(traj,
#'   threeD = TRUE, threeD_col_grad = TRUE,
#'   threeD_grad_start = "#e74c3c",
#'   threeD_grad_end = "#d68910"
#' )
#'
#' traj <- make_trajectory(data = dat, name = "My cool data", system = "ned")
#' plot(traj)
#' plot(traj, col = "orange2", col_start = "pink", col_end = "purple")
#' plot(traj, pch_points_start = 15, cex_points = 3)
#' plot(traj, plot_end_points = FALSE)
#' plot(traj, plot_end_points = FALSE, add_title = FALSE)
#' @importFrom stats qchisq
#' @importFrom graphics layout title text points layout abline
#' @importFrom leaflet addPolylines
plot.trajectory <- function(x, threeD = FALSE, col = "#2980b9", col_start = "#e67e22",
                            col_end = "#e67e22", pch_points_start = 15,
                            pch_points_end = 16, cex_points = 1.5, add_altitude = TRUE,
                            n_split = 6, plot_end_points = TRUE, add_title = TRUE,
                            threeD_line_width = 4, threeD_line_color = "#008080",
                            threeD_col_grad = FALSE, threeD_grad_start = "#008080",
                            threeD_grad_end = "#ab53cf", ...) {
  traj <- x$trajectory
  n <- nrow(traj)
  if (x$system == "ellipsoidal") {
    if (threeD) {
      if (threeD_col_grad) {
        p <- plot_ly(traj,
          x = ~ lat * 180 / pi, y = ~ lon * 180 / pi, z = ~alt, type = "scatter3d", mode = "lines",
          line = list(
            width = threeD_line_width, color = 1:n,
            colorscale = list(c(0, threeD_grad_start), c(1, threeD_grad_end))
          )
        ) %>%
          plotly::layout(
            scene = list(
              xaxis = list(title = "Latitude"),
              yaxis = list(title = "Longitude"),
              zaxis = list(title = "Altitude")
            )
          )
      } else {
        p <- plot_ly(traj,
          x = ~ lat * 180 / pi, y = ~ lon * 180 / pi, z = ~alt, type = "scatter3d", mode = "lines",
          line = list(width = threeD_line_width, color = threeD_line_color)
        ) %>%
          plotly::layout(
            scene = list(
              xaxis = list(title = "Latitude"),
              yaxis = list(title = "Longitude"),
              zaxis = list(title = "Altitude")
            )
          )
      }
      p
    } else {
      label <- c(
        paste("Start - location; time: ", traj[1, 1]),
        paste("End - location; time: ", traj[n, 1])
      )

      leaflet(data = traj) %>%
        addTiles() %>%
        addMarkers(
          lng = ~ lon * 180 / pi,
          lat = ~ lat * 180 / pi, data = x$trajectory[c(1, n), ],
          label = label
        ) %>%
        addPolylines(
          lng = ~ lon * 180 / pi,
          lat = ~ lat * 180 / pi,
          data = x$trajectory
        )
    }
  } else {
    if (threeD) {
      if (threeD_col_grad) {
        p <- plot_ly(traj,
          x = ~x_E, y = ~x_N, z = ~ -x_D, type = "scatter3d", mode = "lines",
          line = list(
            width = threeD_line_width, color = 1:n,
            colorscale = list(c(0, threeD_grad_start), c(1, threeD_grad_end))
          )
        )
      } else {
        p <- plot_ly(traj,
          x = ~x_E, y = ~x_N, z = ~ -x_D, type = "scatter3d", mode = "lines",
          line = list(width = threeD_line_width, color = threeD_line_color)
        )
      }
      p <- p %>%
        plotly::layout(
          scene = list(
            xaxis = list(title = "Local - East"),
            yaxis = list(title = "Local - North"),
            zaxis = list(title = "Local - Up")
          )
        )
      p
    } else {
      if (add_altitude) {
        # to define old par on exit
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        
        par(mfrow = c(1, 2))
      }

      # Compute range
      range_x <- range(traj$x_E)
      range_y <- range(traj$x_N)
      center_x <- mean(range_x)
      center_y <- mean(range_y)
      if (diff(range_x) >= diff(range_y)) {
        delta <- (diff(range_x) - diff(range_y)) / 2
        range_y[1] <- range_y[1] - delta
        range_y[2] <- range_y[2] + delta
      } else {
        delta <- (diff(range_y) - diff(range_x)) / 2
        range_x[1] <- range_x[1] - delta
        range_x[2] <- range_x[2] + delta
      }

      plot(traj$x_E, traj$x_N,
        col = col, type = "l",
        xlab = "Local - East - Coord.",
        ylab = "Local - North - Coord.", xlim = range_x,
        ylim = range_y
      )
      if (plot_end_points) {
        points(traj$x_E[c(1, n)], traj$x_N[c(1, n)], col = c(col_start, col_end), pch = c(pch_points_start, pch_points_end), cex = cex_points)
      }

      if (!is.null(n_split) && add_altitude) {
        index <- round((1:n_split) * n / (n_split + 1))
        for (i in 1:n_split) {
          text(traj$x_E[index[i]], traj$x_N[index[i]] + 0.015 * diff(range(traj$x_N)),
            as.character(round(traj$time[index[i]]), 1),
            cex = 0.8, col = col_start
          )
        }
      }

      if (!add_altitude) {
        if (!is.null(x$name) && inherits(add_title, "logical") && add_title) {
          title(paste("Trajectory: ", x$name))
        }

        if (inherits(add_title, "character")) {
          title(add_title)
        }
      } else {
        if (add_title) {
          title("Trajectory")
        }
      }

      if (add_altitude) {
        plot(traj$time, -traj$x_D,
          col = col, type = "l",
          xlab = "Time (sec)",
          ylab = "Local - Up - Coord."
        )

        if (add_title) {
          title(paste("Altitude profile"))
        }

        if (plot_end_points) {
          points(traj$time[c(1, n)], -traj$x_D[c(1, n)], col = c(col_start, col_end), pch = c(pch_points_start, pch_points_end), cex = cex_points)
        }

        if (!is.null(n_split)) {
          for (i in 1:n_split) {
            text(traj$time[index[i]], -traj$x_D[index[i]] + 0.015 * diff(range(traj$x_D)),
              as.character(round(traj$time[index[i]]), 1),
              cex = 0.8, col = col_start
            )
          }
        }

        par(mfrow = c(1, 1))
      }
    }
  }
}
