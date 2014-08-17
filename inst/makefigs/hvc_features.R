## R CMD BATCH hvc_features.R
## Copied from ~/proj/sangermea/hvc/ellese/2014-06-08/myvio.R
## 2014-08-09

## First need the data.df function.

require(ggplot2)
require(grid)
require(gridExtra)
require(g2chvc)                         #source('analysis_functions.R')

stats.stars <- function(plot.df) {
  ## Adapted from analysis_functions.R
  q.val<-stat.test(plot.df)
  stopifnot(length(q.val)==10)
  star<-rep("", length(q.val))
  star[which(q.val<=0.05)]<-" *"
  star[which(q.val<=0.01)]<-"**"
  star
}

qplot.sje <- function(x, upper=0) {
  ## Draw one of the boxplots for a given feature number X.
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
  
  df1 <- data.frame(age=data.df$age, region=data.df$region, values=values)
  p <- ggplot(df1) +
    geom_boxplot(aes(x=factor(age), y=values, fill=region),
                 lwd=.1, position="dodge", outlier.size=0.5) +
                   theme_classic(base_size=8) + theme(legend.position='none') +
                     theme(axis.line=element_blank()) +
                             xlab("DIV") + ylab(ynames[x]) +
                               theme(axis.line=element_blank(),
                                     axis.ticks = element_line(size=0.2))
  
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

  ## Restrict the range of the axes;  don't use aes() call here.
  ## See http://stackoverflow.com/questions/25327694/how-to-tweak-the-extent-to-which-an-axis-is-drawn-in-ggplot2  
  gpb = ggplot_build(p)
  xrange = range(gpb$panel$ranges[[1]]$x.major_source)
  yrange = range(gpb$panel$ranges[[1]]$y.major_source)
  p = p +
    geom_segment(x=xrange[1], xend=xrange[2], y=-Inf, yend=-Inf, size=0.03) +
    geom_segment(y=yrange[1], yend=yrange[2], x=-Inf, xend=-Inf, size=0.03)
    
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

## Todo: this should come from the package.
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

## upper.bounds stores the maximal value to show on the graph; if zero, there is no limit.
## These values were chosen to minimise the y range whilst mininising the number of outliers
## omitted from the graph.
upper.bounds <- rep(0,11)
upper.bounds[4] = 1.6
upper.bounds[5] = 150
upper.bounds[8] = 10
upper.bounds[9] = 125

plot.order <- c(8:9, 1:7, 10:11)        #rearrange to follow description in paper.
l <- mapply(qplot.sje, plot.order, upper.bounds[plot.order], SIMPLIFY=FALSE)
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


## Compute a table that will show the number of
## arrays per age for each region.
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

pdf(file= 'hvc_features1.pdf', pointsize=10,
    paper = 'a4',
    width=inch(18), height=inch(16))
do.call(grid.arrange, g2)
pushViewport(viewport(0.80, 0.4, 0.2, 0.5))
grid.draw(dummy.legend); popViewport()
pushViewport(viewport())
grid.text(LETTERS[1:12], gp=gpar(fontsize=12),
          x=unit(rep(c(.08, 0.41, 0.75), times=4), "npc"),
          y=unit(rep(c(0.99, 0.75, 0.5, 0.25), each=3) , "npc"))
popViewport()
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

## pdfcrop hvc_features1.pdf


