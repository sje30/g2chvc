## Show example features that we computer.
## R CMD BATCH hvc_rasters.R
require(g2chvcdata)
require(sjemea)
require(rhdf5)



h5.dir <- g2chvcdatadir()

## use tiling coefficient.
options(sjemea.corr.method = "Tiling")
h14 <- h5.read.spikes( paste0(h5.dir, 'TC189-DIV14_A.h5'))
fourplot(h14)

h14$ns <- compute.ns(h14, ns.T=0.003, ns.N=10,sur=100)
##plot(h14$ns, ylab='Count', xlab='Time (s)')
##plot(h14$ns$mean, xlab='Time (s)', ylab='number of active electrodes', main='mean network spike')





axis.ticks <- function(which, lo, hi, step, n) {
  ## Draw major axis from LO to HI with steps of STEP.
  ## In between two major tick marks, draw N minor ticks.
  ## WHICH: 1 for x, 2 for y axis.
  major <- seq(from=lo, to=hi, by=step)
  axis(which, at=major)
  minor.step <- step/(n+1)
  minor <- seq(from=lo, to=hi, by=minor.step)
  minor <- setdiff(minor, major)
  axis(which, at=minor, label=F, tcl=par()$tcl*0.6)
}


plot.one.isihist <- function() {
  allisi <- unlist(sapply(h14$spikes, diff))

  allisi <- diff (h14$spikes$ch_17A_unit_0) #take just a median electrode.

  x <- allisi
  den <- density(log(x))
  den$x <- exp(den$x)
  plot(den, log='x',
       ##xlab='Interspike interval (s)',
       xlab='Frequency (Hz)', type='n',
       main='theta bursting',bty='n',xaxt='n', yaxt='n')
  theta.col <- "blue"
  theta.col <- rgb(0,0,0.7, alpha=0.2)
  theta.col <- "PowderBlue"
  rect(1/10, 0.0, 1/4, 0.35,col=theta.col,border=NA,xpd=NA)
  lines(den)
  axis.ticks(2, 0, 0.3, 0.1, 1)
  axis(1, at=c(1e-3,1e-2,1e-1,1, 1e1,1e2,1e3),
       ##label=c("0.001", "0.01", "0.1", "1", "10", "100", "1000")
       label=c("1000", "100", "10", "1", "0.1", "0.01", "0.001")
       )
  ##axis(1, at=c(0.001,0.01,0.1,1, 10, 100, 1000))
  p = peaks(den$y)
  which(p)
  points(den$x[p], den$y[p],pch=20)
  den$x[p]
}

## Compute ISIs

plotegfeatures <- function() {
  pdf(file='hvc_egfeatures.pdf', width=5, height=8, pointsize=14)
  par(mfrow=c(3,1), las=1, bty='n')
  plot(h14$ns$mean, xlab='Time (s)', ylab='number of active electrodes', main='mean network spike')
  plot.corr.index(h14, main='pairwise correlation')
  plot.one.isihist()
  text(grconvertX(rep(0.04, 3), from='ndc'),
       grconvertY(c(0.3, 0.65, 0.97), from='ndc'),
       c('C', 'B', 'A'), xpd=NA, cex=1.5)
  dev.off()
}



plotegfeatures()
