## Create rasters across development.
## R CMD BATCH hvc_rasters.R
require(g2chvcdata)
require(sjemea)
require(rhdf5)

## Pick 60 seconds of activity to show.  We read in only this minute of data.
t.beg <- 300
t.end <- t.beg + 60


read.h5 <- function(file) {
  h5.dir <- g2chvcdatadir()
  s <- h5.read.spikes( paste0(h5.dir, file), beg=t.beg, end=t.end)
  s
}

c7  <- read.h5('C57_CTX_G2CEPHYS1_TC11_DIV07_A.h5')
c14 <- read.h5('C57_CTX_G2CEPHYS1_TC11_DIV14_A.h5')
c21 <- read.h5('C57_CTX_G2CEPHYS1_TC11_DIV21_A.h5')

h7  <- read.h5('TC189-DIV07_A.h5')
h14 <- read.h5('TC189-DIV14_A.h5')
h21 <- read.h5('TC189-DIV21_A.h5')

rplot <- function(s, div.lab='', ylab='', scalebar=FALSE) {
  ## Draw one raster plot.
  elecs <- 1:6
  t.beg <- 300; t <- t.end <- t.beg+60
  plot(s, which=elecs, beg=t.beg, end=t.end, main='', ylab='',
       xlab='', xaxt='n')

  if (nchar(div.lab)>0) {
    mtext(div.lab, side=3, line=0.6, font=2)
  }

  if (nchar(ylab)>0) {
    mtext(ylab, side=2, line=0.6, font=2)
  }
  if (scalebar) {
    segments(t.beg, -0.06, t.beg+10, lend='butt', xpd=NA, lwd=2)
  }
}


plotrasters <- function() {
  ## Draw all six rasters.
  par(mfrow=c(2,3), mar=c(.1,1, 1, 1), oma=c(0, 1, 1,0))
  rplot(h7,  'DIV 7', ylab='HPC', scalebar=TRUE)
  rplot(h14, 'DIV 14')
  rplot(h21, 'DIV 21')
  rplot(c7, ylab='CTX')
  rplot(c14)
  rplot(c21)
}


pdf(file='hvc_rasters.pdf', width=7, height=3, pointsize=14); plotrasters(); dev.off()

##svg(file='hvc_rasters.svg', width=7, height=5, pointsize=14); plotrasters(); dev.off()
