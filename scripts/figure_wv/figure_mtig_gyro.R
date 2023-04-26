# clean ws
rm(list=ls())

# imort libraries
library(tikzDevice)
library(magrittr)

# plot for other signal
load(file = "scripts/figure_wv/mtig100hrz.rda")

# check frequency
str(mtig100hrz)
attr(mtig100hrz, "freq")
lapply(mtig100hrz, attributes)
frequency_per_sec = attr(mtig100hrz[[1]], "freq") %>% as.numeric()

# there is a total of 1048569 data point, so with a frequency of 100, that is roughly 174 min
length(mtig100hrz[[4]][[3]]$data)
length(mtig100hrz[[4]][[3]]$data) / 100  / 60 



# begin figure
tikz("scripts/figure_wv/plot/fig_mtig_100hrz.tex",
     width = 6, height = 2.5, standAlone = TRUE,
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


par(mfrow=c(1,1))
space_left_2 = 5.5
space_below=4.5
par(mar=c(space_below,space_left_2,3,1))

nbr_min = 60
nbr_sec_per_min = 60
xx = seq(1, nbr_min *  nbr_sec_per_min *frequency_per_sec, by = frequency_per_sec)
yy = mtig100hrz[[4]][[3]]$data[xx]
length(xx)
plot(x=seq(length(xx)), y=yy, las=1, xaxt="n",
     type="l", ylab="", xlab="")
mtext(side=3, text = "Gyroscope Signal", line=1)

ticks_label = seq(0, 60, by=5)
ticks_pos = ticks_label * nbr_sec_per_min
axis(side = 1, at = ticks_pos, labels = ticks_label)
mtext(side=1, text = "Time [min]", line = 2.3)
mtext(side=2, text = "rad/sec", line = 4.5)
for(i in ticks_pos){
  abline(v=i, col="grey90")
}

for(i in axTicks(2)){
  abline(h=i, col="grey90")
}
lines(x=seq(length(xx)), y=yy)

dev.off()

system("pdflatex -output-directory='scripts/figure_wv/plot/'  scripts/figure_wv/plot/fig_mtig_100hrz.tex")


