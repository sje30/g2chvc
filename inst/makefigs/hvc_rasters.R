## Create rasters across development.

require(g2chvcdata)
require(sjemea)
require(rhdf5)


hvc_rasters <- function() {
  ## Generate rasters() plot.
  h5.dir <- g2chvcdatadir()

  h5.files <- list.files(path=h5.dir, pattern='.h5',
                         full.names=TRUE, recursive=TRUE)

  f <- h5.files[100]
  s <- h5.read.spikes(f)

  t.beg <- 100; t.end <- 160
  pdf(file='hvc_rasters.pdf')
  plot(s, which=1:10, beg=t.beg, end=t.end)
  dev.off()
  
}
