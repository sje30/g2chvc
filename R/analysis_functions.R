#Contains functions for analysis_main.R script

#Creates list containing spike times for all the hdf5 files
create.spikes<-function(mea.data.dir)  { 
  mea.data.files <- make.meafile.cache(mea.data.dir)
  file.list<-mclapply(mea.data.files[,"files"], sort.data)
  file.df<-do.call("rbind", file.list)
  s.list.all<-NULL
  s.list.all$ctx<-mclapply(ages, function(x) spikes(x, "ctx", file.df))
  s.list.all$hpc<-mclapply(ages, function(x) spikes(x, "hpc", file.df))
  s.list.all
}
  
#Main analysis function. Input is type ("burst", "network.spike", "fir.rate", "corr" or "theta.burst") and list of 
#spike trains for all data. Output is data frame of the desired type of analysis data.
spike.analyse<-function(type, s.list.all) {
  data.df<-NULL
  for (i in 1:2) {
    ret<-mclapply(s.list.all[[i]], function(x) analysis.fn(x, type))
    ret.df<-make.df(ret, regions[i])
    data.df<-rbind(data.df, ret.df)
  }
  data.df
}

#Function to read in data for all arrays of a specific age and region
 spikes<-function(age1, region1, file.df) {
   file.subset<-subset(file.df, age==age1 & region==region1)[,"file"]
   file.strings<-sapply(file.subset, toString)
   s.list<-lapply(file.strings, h5.read.spikes)
 }
 
#Function to sort hdf5 data. Input is file name, output is data frame containing the 
# file name, age and region of that data file
sort.data<-function(file.name) {
  file.age<- h5read(file.name,"meta/age")
  file.region<- h5read(file.name,"meta/region")
  file.df<-data.frame(file=toString(file.name), age=file.age, region = file.region)
}


#Converts list of statistics into one data frame, containing age and region variables
make.df<-function(data.list, region){
  list.dim<-sapply(data.list, dim)[1,]
  age.vec<-NULL
  for (i in 1:length(ages)) {
    age.vec<-append(age.vec, rep(ages[i], list.dim[i]))
  }
  data.df<-do.call("rbind", data.list)
  data.df$age<-age.vec
  data.df$region<-rep(region, dim(data.df)[1])
  data.df
}

#Performs specified analysis. Input is a list of arrays spike times, and type ("burst", "network.spike", 
#"fir.rate", "corr" or "theta.burst"). Output is data frame of the results of the specified analysis
analysis.fn<- function(s.list, type) {
  if (type=="burst") { 
    ret<- burst.analysis(s.list)
  } else if (type=="network.spike") {
    ret<-as.data.frame(network.spikes(s.list))
  } else if (type=="fir.rate") {
    rate.list<- lapply(s.list, function(x) median(x$meanfiringrate))
    rate.df<-as.data.frame(unlist(rate.list))
    b.list<-lapply(s.list, function(x) spikes.to.bursts(x, "mi"))
    burst.fr<-sapply(b.list, burst.fir.rate)
    fr.med<-unlist(sapply(burst.fr, function(y) median(y, na.rm=TRUE)))
    burst.fr.df<-as.data.frame(fr.med)
    ret<-burst.fr.df
    ret<-cbind(rate.df, burst.fr.df)
  } else if (type=="corr"){ 
    times<-c(0.05, 0.005, 0.001)
    ci.mean.df<-NULL
    for (i in 1:length(times)){
      mn.ci<-lapply(s.list, function(x) mean.ci(x, times[i]))
      ci.mean<-unlist(mn.ci)
      ci.mean.df<-cbind(ci.mean.df, ci.mean)
    }
    ret<-as.data.frame(ci.mean.df)
  } else if (type=="theta.burst") {
    th.burst<-unlist(lapply(s.list, tburst.elec))
    ret<-as.data.frame(th.burst)
  }
  names(ret)<-df.names(type)
  ret
}



#Specifies the column names of data-frame for each type of analysis
df.names<-function(type) {
   if (type=="burst") {
    names<-c("Burst Rate (per min)", "Burst Duration (sec)", "% Spikes in Bursts", "CV of IBI")
   } else if (type=="network.spike") {
     names<-c("Network spike rate (per min)", "Network spike peak", "Network spike duration (sec)")
   }else if (type=="fir.rate"){
     names<-c("Firing rate (Hz)", "Within burst firing rate (Hz)")
   } else if (type=="corr") {
    names<-c("dt = 0.05", "dt= 0.005", "dt=0.001")
   } else if (type=="theta.burst") {
    names<-c("Fraction of electrodes theta bursting")
   } 
}

#Function to calculate burst statistics
#Input is list of spike trains, ouput is data frame of burst statistics
burst.analysis <- function(s.list) {
  b.list<- lapply(s.list, function(x) spikes.to.bursts(x, "mi"))
  for (j in 1:length(s.list)) {
    s.list[[j]]$allb <- b.list[[j]]
  }
  bsum.list<-lapply(s.list, calc.burst.summary)
  stat.sum<-burst.stats(bsum.list)
  stat.sum
}

#Calculates median bursts per minute, burst duration, percent of spikes in bursts and 
#CV of IBI from burst summary list
burst.stats<-function(bsum.list){
  bursts.pm<-sapply(sapply(bsum.list, "[[", 6), function(x) median(x[!x==0]))
  burst.dur <- sapply(sapply(bsum.list, "[[", 8), function(x) median(x[!x==0]))
  s.in.b<- sapply(sapply(bsum.list, "[[", 12), function(x) median(x[!x==0], na.rm=TRUE))
  cv.IBI <- sapply(sapply(bsum.list, "[[", 19), function(x) median(x[!x==0], na.rm=TRUE))
  stat.sum<-data.frame(bursts.pm=bursts.pm, burst.dur=burst.dur, s.in.b=s.in.b, cv.IBI=cv.IBI)
}

#Function to calculate network spikes statistics
#Input is list of spikes, output is data frame containing median peak value and duration of network spikes
network.spikes <- function(s.list) {
  n.list<-lapply(s.list, function(x) compute.ns(x, ns.T=0.003, ns.N=10, sur=100))
  ns.stat.sum<-NULL
  for (j in 1:length(n.list)) {
    if (is.null(n.list[[j]]$measures)==0) {
      ns.meds<- apply(n.list[[j]]$measures, 2, median)[3:4]
      rec.durn <- (s.list[[j]]$rec.time[2] - s.list[[j]]$rec.time[1])/60
      ns.all.stat<-data.frame(ns.rate=n.list[[j]]$brief[1]/rec.durn, peak.val=ns.meds[1], durn=ns.meds[2])
    } else {
      ns.all.stat<-data.frame(ns.rate=0, peak.val=NA, durn=NA)
    }
    ns.stat.sum <- rbind(ns.stat.sum, ns.all.stat)
  }
  rownames(ns.stat.sum) =NULL
  ns.stat.sum
}

#Function to calculate within burst firing rate. Input is list of bursts, 
#output is vector of firing rates
burst.fir.rate<-function(bursts) {
  not.nul<- which(sapply(bursts, function(y) dim(y)[1])>0)
  firing.rate<-logical(0) 
  for (j in not.nul) { 
    bl<-  bursts[[j]][,"len"]/ bursts[[j]][,"durn"]
    bl.mn<-mean(bl)
    firing.rate<-append(firing.rate, bl.mn)
  }
  firing.rate
}

#Function to calculate the mean of the upper triangular correlation matrix. Input is list of spike times
#from one array and dt value to be used
mean.ci<-function(s, time) {
  m<-tiling.allpairwise(s, dt=time)
  mean(m[upper.tri(m)], na.rm=TRUE)
}


#Function to calculate theta bursting. Input is vector of spike times from one electrode, output is TRUE if
#theta bursting is occuring at this electrode, otherwise false
smooth.isi.elec<-function(s) {
  allisi <- diff(unlist(s))
  x <- allisi
  if (length(x)>1) {
    den <- density(log(x))
    den$x <- exp(den$x)
    p <- peaks(den$y)
    pks<-which(p)
    theta.reg<-which(den$x<=(1/4) & den$x>=(1/10))
    ret<-sum(intersect(pks, theta.reg))>0 
  }else{
    ret<-FALSE
  }
  ret
}

#Calculates fraction of electrodes theta bursting on any array. Input is list of spike trains from one array,
#output is number representing fraction of theta bursting on that array
tburst.elec<-function(s){
  spks<-s$spikes
  t.burst<-lapply(spks, smooth.isi.elec)
  theta.frac<- sum(unlist(t.burst))/length(t.burst)
}


#Performs wilcox test and false discovery rate adjustment. Output is q-value vector
#containing fdr adjusted p-values from compairing hpc and ctx data for each DIV
stat.test<-function(data.df) {
  p.val<-sapply(ages, function(x) wilcox.fn(data.df, x))
  q.val<-p.adjust(p.val, method="fdr")
}

#Performs wilcox test to compare cortex and hippocampal data. Input is data frame and 
#age at which test is to be performed, output is single p-value
wilcox.fn <- function(data.df, age1) {
  p.value<-wilcox.test(n~region, data=data.df, subset = (age==age1))$p.value
}

#Creates pdf containing strip charts for each of the quantities. Input is data frame of quantities to be plotted
#and type of analysis. If stats=TRUE, FDR adjusted p-values are calculated and significance levels displayed on 
##the plots, where ** is significance at 1% and * is significance at 5%
plot.fn<-function(data.df, type, stats=FALSE) {
  fname<-paste(type, "pdf", sep=".")
  pdf(file = fname, width = 15, height = 12)
  par(mfrow = c(2, 2))
  main<-NULL
  nms<-names(data.df)
  lnth<-length(nms)-2
  if (type=="corr") {
    main<-nms[1:lnth]
    nms<-rep("Mean correlation", lnth)
  }
  for (i in 1:lnth) {
    plot.df<-data.frame(n=data.df[,i], region = data.df$region, age=data.df$age)
    star<-rep("",10)
    if (stats==TRUE) {
      q.val<-stat.test(plot.df)
      star[which(q.val<=0.05)]<-"*"
      star[which(q.val<=0.01)]<-"**"
    }
    stripchart(n~region*age, data= plot.df ,  vertical=TRUE, method="jitter", col=c("indianred1", "skyblue"), pch=16, ylab=nms[i], xaxt="n", xlab = "DIV", main=main[i])
    axis(1, at = seq(0.5, 20.5, 2), labels = FALSE, tick = TRUE)
    axis(1, at=seq(1.5, 19.5, 2), labels = ages[1:10], tick=FALSE)
    par(las=1)
    boxplot(n~region*age, data= plot.df,  outline = FALSE, boxlty = 0, whisklty = 0, staplelty = 0, add=TRUE, medcol=c("red3", "blue4"), axes=FALSE)
    mtext(star, side=3, line=0, at=seq(1.5, 19.5, 2), cex=1.2)
    legend("topleft",c("cortex", "hippocampus"),  bty="n", col=c("indianred1","skyblue"),  pch = c(16,16))
  }
  dev.off()
}









