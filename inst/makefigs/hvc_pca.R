# Rscript hvc_pca.R
# 2014-09-25
require(g2chvc)

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

if (require(RColorBrewer)) {
  mypallete = brewer.pal(3, "Set1")
  ctx.col = mypallete[1]; hpc.col = mypallete[2]
} else {
  ## hardcode if you don't have RColorBrewer
  ctx.col = "#E41A1C"; hpc.col = "#377EB8"
}

draw.pca.scree.age = function(age1) {
  ## Draw PCA and scree plot for a given age.
  data.df.1 = subset(data.df, age==age1)

  mat = as.matrix(data.df.1[,-(1:2)])
  pca = prcomp(mat, scale=TRUE)

  pca.rot = pca$rotation[,1:2]
  m2 = pca$x[,1:2]
  plot(m2, col=ifelse(data.df.1=='ctx', ctx.col, hpc.col),
       pch=20, ##asp=1,
       xlab='',ylab='',
       xaxt='n', yaxt='n',
       main=sprintf("DIV %d",age1))
  if (age1==7) {
    legend('topright', legend=c('CTX', 'HPC'), pch=20,
           col=c(ctx.col, hpc.col))
    title(ylab='PC2', line=2.5, xpd=NA)
  }
  if (age1==14) {
    title(xlab='PC1', line=1, xpd=NA)
  }
  ##plot(pca, npcs=11, main='')

  percentage = stats:::summary.prcomp(pca)$importance[3, ]
  plot(percentage, type='l', ylim=c(0,1), bty='n',
       yaxt='n',
       xlab='', xaxt='n', ylab='')
  points(percentage, pch=20)
  if(age1==7) {
    title(ylab='cumulative fraction of variance', line=2.5, xpd=NA)
  }
  if(age1==14) {
    title(xlab='principal component', line=1.7, xpd=NA)
  }
  
  ##axis(1, at=c(1,11))
  axis.ticks(1, 1, 11, 10,10)
  axis.ticks(2, 0, 1, 0.5,4)
}



## Load in the data from the csv.
data.df <- read.csv(system.file("stored/hvcfeatures.csv", package="g2chvc"),
                    row.names=1)

ages = unique(data.df$age)


## par(mfrow=c(4,3))
## lapply(ages, draw.pca.age)



pdf(file="hvc_pca.pdf",
    width=inch(14), height=inch(10), pointsize=12)
par(mfcol=c(2,3),mar=c(2.8, 2.6, 1.3, 0.3), oma=c(0,1,0,0),
    mgp=c(1.0, 0.8, 0), las=1)
lapply(c(7,14,21), draw.pca.scree.age)
dev.off()


