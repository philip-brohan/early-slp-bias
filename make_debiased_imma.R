# Make a set of IMMA files identical to R3.0 except with
#  ship-by-ship bias adjustments applied to SLP.

library(IMMA)
bias.ship<-readRDS('bias.ship.Rdata')

for(year in seq(1800,1870,1)) {
  for(month in seq(1,12)) {
    i.file<-sprintf("%s/icoads_3.0/IMMA1_R3.0.0_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),year,month)
    if(!file.exists(i.file)) next
    obs<-ReadObs(i.file)
    ids<-unique(obs$ID)
    for(f in ids) {
       w<-which(obs$ID==f)
       if(length(w)==0) next
       if(is.null(bias.ship[[f]])) is.na(bias.ship[[f]])<-TRUE
       obs$SLP[w]<-obs$SLP[w]-bias.ship[[f]]
     }
    o.file<-gzfile(sprintf("%s/icoads_3.0_SLP_debiased/IMMA1_R3.0.0_SLPD_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),year,month),open='w')
    WriteObs(obs,o.file)
    close(o.file)
  }
}
