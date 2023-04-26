rm(list = ls())

# load data and functions
load("scripts/figure_wv/fit_data.RData")
source("scripts/figure_wv/functions.R")
load("scripts/figure_wv/kvh1750.acc.rda")

# load package
library(tikzDevice)
library(magrittr)


# check structure of the file
str(kvh1750.acc)
str(kvh1750.acc$`Acc. X`)

# compute aveerage of signal
for(i in seq(6)){
  print(mean(kvh1750.acc$`Acc. X`[[i]]$data))
}
for(i in seq(6)){
  print(mean(kvh1750.acc$`Acc. Y`[[i]]$data))
}
for(i in seq(6)){
  print(mean(kvh1750.acc$`Acc. Z`[[i]]$data))
}


# get frequency
frequency_per_sec = attr(kvh1750.acc$`Acc. X`, "freq") %>% as.numeric()

# define frequency as 200hertz
frequency_per_sec = 200
space_left = 4

# produce figure
tikz("scripts/figure_wv/plot/example.tex",
     width = 6, height = 6.5, standAlone = TRUE,
     packages = c(
       "\\usepackage{tikz}",
       "\\usepackage[active,tightpage,psfixbb]{preview}",
       "\\PreviewEnvironment{pgfpicture}",
       "\\setlength\\PreviewBorder{0pt}",
       "\\usepackage{amssymb}",
       "\\usepackage{bm}", "\\usepackage{amsthm}", "\\usepackage{amsbsy}",
       "\\usepackage{amsbsy}",
       "\\usepackage{amsbsy}",
       "\\usepackage{amsfonts}",
       "\\usepackage{amsmath}"
     )
)


cex_param <- .6
par(cex = cex_param)

# define layout
m <- matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE)
layout(mat = m, heights = c(.4, 1))
par(mar=c(3,space_left,3,1))

# define cex axis
cex_axis=.8

# define which observation to plot
length(kvh1750.acc$`Acc. X`[[1]]$data) 
# there are a million data points with a frequency of 1000, so roughly 15 min of data.
# we now consider a frequency of 200 hertz
length(kvh1750.acc$`Acc. X`[[1]]$data)  / frequency_per_sec / 60
number_of_min = 60
sec_per_min = 60
xx = seq(1, number_of_min*sec_per_min* frequency_per_sec, by = frequency_per_sec)
yy = kvh1750.acc$`Acc. X`[[1]]$data[xx] * 1e3
plot(
  x= seq(length(xx)),
  y=yy, las=1, xaxt="n",
     type="l", ylab="", 
  yaxt="n"
  )

axis(side=2, las=2, cex=cex_axis, at = axTicks(2), labels = axTicks(2))
mtext(side=3, text = "Accelerometer Signal", line=1)
xaxis_ticks = seq(0, number_of_min, by = 5)
xaxis_ticks_pos = xaxis_ticks*sec_per_min
axis(side = 1, at = xaxis_ticks_pos, labels = xaxis_ticks)
mtext(side=1, text = "Time [min]", line = 2.3)
mtext(side=2, text = "Specific force [mg]", line = 2.9)

for(i in xaxis_ticks_pos){
  abline(v=i, col="grey90")
}
for(i in axTicks(2)){
  abline(h=i, col="grey90")
}


lines(  x= seq(length(xx)),
        y=yy)
par(mar=c(4.5,space_left,3,1))

magic_plot(data = list(d), mdls = list(m2, m3), scales = 2^(seq(1, 19)), 
           decomp = T, yl = c(1e-10, 1e-6), freq = 200, 
           ylab = "Wavelet Variance $\\boldsymbol{\\nu}$ [$\\text{g}^2$]")
mtext(side=3, text = "Haar Wavelet Representation", line=1)

legend("topright",
  bty = "n",
  c(
    "Empirical WV $\\hat{\\boldsymbol{\\nu}}$",
    "CI 95\\%",
    "GMWM Model $\\boldsymbol{\\nu}(\\boldsymbol{\\theta})$",
    "AVLR Model $\\boldsymbol{\\nu}(\\boldsymbol{\\theta})$",
    "GM $\\hat\\tau_c = 0.90$ s",
    "GM $\\hat\\tau_c = 23.87$ s",
    "GM $\\hat\\tau_c = 5.22$ h",
    "WN"
  ),
  lt = c(1, NA, 1, 1, 2, 2, 2, 2),
  lw = c(2, NA, 2, 2, 2, 2, 2, 2),
  col = c(
    "#fa9891",
    "#feeae9",
    "#d7bb54",
    "#53b400",
    "#a3e8d8",
    "#15bced",
    "#c5b3ff",
    "#fc9be6"
  ),
  pch = c(NA, 15, NA, NA, NA, NA, NA, NA),
  pt.cex = c(NA, 2, NA, NA, NA, NA, NA, NA)

)


dev.off()
system("pdflatex -output-directory='scripts/figure_wv/plot/'  scripts/figure_wv/plot/example.tex")





