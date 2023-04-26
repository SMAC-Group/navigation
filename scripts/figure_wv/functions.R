magic_plot <- function(data = list(), mdls = list(), scales, f=function(x){x}, lw = NA, title = NA, legend = NULL, ylab = NA, yl = NA, decomp = FALSE, freq=NA) {
  if (is.na(lw)) {
    lw = rep(1, length(mdls))
  }

  ws = list()
  decomps = list()

  for (i in seq_along(data)) {
    ws[[i]] = gmwm::wvar(data[[i]])
  }

  for (i in seq_along(mdls)) {
    mdl = f(mdls[[i]])
    # print(mdl)
    ws[[i+length(data)]] = list("scales" = scales, "variance" = gmwm::theoretical_wv(theta = mdl$theta, desc = mdl$desc, objdesc = mdl$obj.desc, scales))

    if (decomp == TRUE && i == 1) {
      decomps[[i]] = list("scales" = scales, "variances" = gmwm::decomp_theoretical_wv(theta = mdl$theta, desc = mdl$desc, objdesc = mdl$obj.desc, scales))
    }
  }

  if (is.na(yl[1])) {
    yl = yl(ws, function(x){x$variance})
  }

  plot(NA,
       xlim = c(min(scales), max(scales)),
       ylim = yl,
       log="xy",
       main = title,
       xlab = "Scale $\\tau$ [s]",
       ylab = ylab,
       xaxt = "n",
       yaxt = "n")

  yt = 10^(-1:-10)

  if (is.na(freq)) {
    axis(1, at=scales, labels=log2(scales))
    abline(h = yt, v = scales, col = "grey", lt=2  )
  } else {
    l = c(floor(log10(scales[1]/freq)), ceiling(log10(tail(scales,1)/freq)))
    e = seq(l[1],l[2])
    xx = 10^e*freq

    axis(1, at=xx, labels=lapply(e, function(x) {sprintf('$10^{%.0f}$',x)}))
    abline(h = yt, v = xx, col = "grey", lt=2  )

  }

  axis(2, at=yt, labels=lapply(yt, function(x) {sprintf('$10^{%.0f}$',log10(x))}))

  for (i in seq_along(ws)) {
    if ('ci_low' %in% names(ws[[i]])) {
      polygon( c(ws[[i]]$scales,rev(ws[[i]]$scales)), c(ws[[i]]$ci_low, rev(ws[[i]]$ci_high)), col = cols_trans[1], border = NA)
    }

    lines(ws[[i]]$scales, ws[[i]]$variance, col=cols[i], lw=2)
    print(i)
    if (i == 1) {
      points(ws[[i]]$scales, ws[[i]]$variance, col=cols[i], pch=1, cex = 1.5, lw=2)
    } else {
      points(ws[[i]]$scales, ws[[i]]$variance, col=cols[i], pch=16, lw=2)
    }

  }

  for (i in seq_along(decomps)) {
    for (j in 1:dim(decomps[[i]]$variances)[2]) {
      lines(decomps[[i]]$scales, decomps[[i]]$variances[,j], col=cols[j+3], lt=2, lw=2)
    }
  }


  # if (!is.null(legend)) {
  #   legend("topright", horiz = F, legend=legend, col=1:(length(data)+length(mdls)), lt=1)
  # }
}

gg_color_hue <- function(n, alpha) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

cols = gg_color_hue(7, alpha = 1)
cols_trans = gg_color_hue(7, alpha = 0.15)
