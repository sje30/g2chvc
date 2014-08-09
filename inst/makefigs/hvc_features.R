## R CMD BATCH hvc_features.R
## Copied from ~/proj/sangermea/hvc/ellese/2014-06-08/myvio.R
## 2014-08-09

## First need the data.df function.
## 10 should be length(ages) in most cases...

require(ggplot2)
require(grid)
require(gridExtra)
require(g2chvc)                         #source('analysis_functions.R')

stats.stars <- function(plot.df) {
  ## Adapted from analysis_functions.R
  q.val<-stat.test(plot.df)
  star<-rep("",10)
  star[which(q.val<=0.05)]<-" *"
  star[which(q.val<=0.01)]<-"**"
  star
}

qplot.sje <- function(x, upper=0) {
  ## X is the row number to process.
  values <- data.df[,x+2]
  if (x==11) {
    # multiply theta bursting by 100 for percentage.
    values <- values*100
  }
  s.df <- data.frame(age=data.df$age, region=data.df$region, n=data.df[,x+2])
  star <- stats.stars(s.df)
  ages1 <- unique(data.df$age)
  star.formatter <- function(x.lab) {
    age.id <- which(x.lab==ages1)
    lab <- sprintf('%s\n%s', x.lab, star[age.id])
    lab
  }
  ## to move the legend position.
  ##theme(legend.position=c(0.1,0.9), legend.direction="horizontal") +

  
  ## p <- qplot(factor(age), values, fill=region, data=data.df, 
  ##            geom="boxplot", position="dodge", outlier.size=1.0, fatten=1) +
  ##              theme_bw(base_size=8) + 
  ##                theme(legend.position='none') +
  ##                  xlab("DIV") + ylab(names(data.df)[x+2])

  df1 <- data.frame(age=data.df$age, region=data.df$region, values=values)
  p <- ggplot(df1) +
    geom_boxplot(aes(x=factor(age), y=values, fill=region),
                 lwd=.1, position="dodge", outlier.size=0.5) +
                   theme_bw(base_size=8) + theme(legend.position='none') + 
                   xlab("DIV") + ylab(ynames[x]) +
                     theme(axis.ticks = element_line(size=0.2))
  
  
  ## pold <- qplot(factor(age), values, fill=region, data=data.df, 
  ##            geom="boxplot", position="dodge", outlier.size=1.0, fatten=1) +
  ##              theme_bw(base_size=8) + 
  ##                theme(legend.position='none') +
  ##                  xlab("DIV") + ylab(names(data.df)[x+2])

    
  ## or lets remove the outliers.

  if (upper>0) {
    ## let's omit some outliers.
    outliers = sum(values > upper)
    p = p + coord_cartesian(ylim=c(0, upper)) +
      annotate("text", x=1.2, y=(upper*0.95), size=2,
               hjust=0, ##colour="red",
               label=sprintf("(%d outliers)", outliers))
    
    cat(printf("Removing %d outliers\n", outliers))
  }
  p = p + scale_x_discrete(label=star.formatter)

  p
  
}

g_legend <- function(a.gplot){
  ## http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

######################################################################
## end of functions.

load("~/proj/sangermea/hvc/ellese/2014-06-08/features.Rda")  ## we need data.df


ynames1 <- names(data.df)[-(1:2)]
ynames <- c("burst rate (/min)", "burst duration (s)", "% spikes in burst",
            "CV of IBI", "network spike rate (/min)", "network spike peak", "network spike duration (s)",
            "firing rate (Hz)", "within burst firing rate (Hz)", "mean correlation",
            "% theta bursting")

## http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
## modifying values plotted -- nice!

ages <- unique(data.df$age)             #is this created elsewhere?

temp = qplot.sje(1)
##l <- lapply(1:11, function(x) { qplot.sje(x)})
upper.bounds <- rep(0,11)
upper.bounds[4] = 1.6
upper.bounds[5] = 150
upper.bounds[8] = 10
upper.bounds[9] = 125
l <- mapply(qplot.sje, 1:11, upper.bounds, SIMPLIFY=FALSE)
##plot(l[[4]])

##l <- list(qplot.sje(1), qplot.sje(2))
##plots <- lapply(1:3, qplot.sje, SIMPLIFY=FALSE)

## pdf(file= 'features_box.pdf', pointsize=10,
##     paper = 'a4',
##     width=inch(18), height=inch(16))
## do.call(grid.arrange, l)
## dev.off()


## get the max width and apply it to all of the plots.
## http://stackoverflow.com/questions/13656642/r-align-plot-areas-in-ggplot
g <- lapply(l, function(x) {ggplot_gtable(ggplot_build(x))})
widths <- lapply(g, function(x) x$widths[2:3])
maxwidth <- do.call("unit.pmax", widths)


g2 <- lapply(g, function(x) {x$widths[2:3] <- maxwidth; x})



## Generate a dummy plot...
dummy <- l[[1]] + theme(legend.position=c(0.9,0.1), legend.direction="horizontal")
dummy.legend <- g_legend(dummy)


## Compute a table.
table <- t(table(data.df$age, data.df$region))
mytable <- tableGrob(table, name="name",
                     padding.h = unit(2, "mm"),
                     padding.v = unit(2, "mm"),
                     gpar.corefill = gpar(fill="white", col=NA),
                     gpar.rowfill = gpar(fill="white", col=NA),
                     gpar.colfill = gpar(fill="white", col=NA),
                     gpar.coretext =gpar(fontsize=6),
                     gpar.coltext=gpar(fontsize=6, fontface="bold"),
                     gpar.rowtext=gpar(fontsize=6, fontface="bold"))

pdf(file= 'features_box2a.pdf', pointsize=10,
    paper = 'a4',
    width=inch(18), height=inch(16))
do.call(grid.arrange, g2)
pushViewport(viewport(0.80, 0.4, 0.2, 0.5))
grid.draw(dummy.legend); popViewport()
pushViewport(viewport(0.83, 0.1, 0.28, 0.08))
grid.draw(mytable);
##grid.rect();
grid.text("#arrays per age", gp=gpar(fontsize=6), y=1.1)
##grid.segments(rep(0,10), (1:10)/10, rep(1,10), (1:10)/10, gp=gpar(lwd=0.1))
ys <- c(0.87, 0.65, 0.05); xs <- ys*0
grid.segments(xs, ys, 1+xs, ys, gp=gpar(lwd=0.1))
popViewport()
dev.off()

## End of code...


q()

