## Version 2 of showing the example features.
##
## R CMD BATCH hvc_egfeatures2.R
require(g2chvcdata)
require(sjemea)
require(rhdf5)

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


plot.one.isihist <- function(s, n, show.title=TRUE) {
  ## If n is missing take the median train.
  if (missing(n)) {
    n <- order(s$nspikes)[30]
  }
  
  ##allisi <- unlist(sapply(h14$spikes, diff))
  ##allisi <- diff (h14$spikes$ch_17A_unit_0) #take just a median electrode.
  allisi <- diff (s$spikes[[n]])
  if (show.title) {
    subtitle <- sprintf("electrode %d nISI %d\n", n, length(allisi))
  } else {
    subtitle = ""
  }
  x <- allisi
  if (length(x)> 5) {
    den <- density(log(x))
    den$x <- exp(den$x)
    plot(den, log='x',
         ##xlab='Interspike interval (s)',
         ylab='density',
         xlab='frequency (Hz)', type='n',
         main=subtitle,bty='n',xaxt='n', yaxt='n')
    ##theta.col <- "blue"
    ##theta.col <- rgb(0,0,0.7, alpha=0.2)
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
}
######################################################################
## End of functions
######################################################################


h5.dir <- g2chvcdatadir()

## use tiling coefficient.
options(sjemea.corr.method = "Tiling")
h14 <- h5.read.spikes( paste0(h5.dir, 'TC189-DIV14_A.h5'))
median.train <- order(h14$nspikes)[30]
plot.one.isihist(h14)

## Check a cortical recording.
c14 <- h5.read.spikes( paste0(h5.dir, 'C57_CTX_G2CEPHYS1_TC11_DIV14_A.h5'))
plot.one.isihist(c14)

h14$ns <- compute.ns(h14, ns.T=0.003, ns.N=10,sur=100)
##plot(h14$ns, ylab='Count', xlab='Time (s)')
##plot(h14$ns$mean, xlab='Time (s)', ylab='number of active electrodes', main='mean network spike')



## Compute ISIs

plotegfeatures <- function() {
  pdf(file='hvc_egfeatures.pdf', width=inch(8.5), height=inch(13), pointsize=14)
  par(mfrow=c(3,1), las=1, bty='n', mar=c(3.5,3,0.5,0.5), mgp=c(1.8, 0.6, 0))
  plot(h14$ns$mean, xlab='Time (s)', ylab='#active units', yaxt='n', ylim=c(0,20),
       xaxt='n',
       main='') ##main='mean network spike'
  axis.ticks(1, -0.3, 0.3, 0.3, 2)
  axis.ticks(2, 0, 20, 10, 1)
  plot.corr.index(h14, ,pch=20, cex=0.5, main='', # 'pairwise correlation'
                  dot.col='black',
                  yaxt='n',
                  xaxt='n', xlim=c(0, 1600),
                  show.fit=FALSE, ylabel='correlation', show.method=FALSE)
  axis.ticks(1, 0, 1600, 800, 3)
  axis.ticks(2, 0, 1, 0.5, 1)
  abline(h=mean(h14$corr$corr.id[,"corr"]), col='grey')
  plot.one.isihist(h14, median.train, show.title=FALSE)
  text(grconvertX(rep(0.03, 3), from='ndc'),
       grconvertY(c(0.33, 0.67, 0.98), from='ndc'),
       c('C', 'B', 'A'), xpd=NA, cex=1.5)
  dev.off()
}

plotegfeatures()


## Check to see which electrodes have theta bursting and which don't.
## Can we plot more than one trace on a plot.
## pdf('allplots2.pdf')
## par(mfrow=c(4,4))
## lapply(1:h14$NCells, plot.one.isihist)
## dev.off()


## plot.corr.index(s, pch=20, ylab='correlation')

## dists = s$corr$corr.id[, "dist"]
## corrs = s$corr$corr.id[, "corr"]
## plot(dists, corrs, pch=20, bty='n',
##      las=1,
##      ylab='correlation',
##      xlab=expression(paste("intercell distance (", mu, "m)")))
## mean.corrs = mean(corrs)
## abline(h=mean.corrs, lty=2)
# spike time tiling coefficient
