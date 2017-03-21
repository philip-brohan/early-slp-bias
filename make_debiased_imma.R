# Make a set of IMMA files identical to R3.0 except with
#  ship-by-ship bias adjustments applied to SLP.

library(IMMA)
bias.ship<-readRDS('bias.ship.Rdata')
bias.year<-readRDS('bias.year.deck.Rdata')

for(year in seq(1800,1870,1)) {
  for(month in seq(1,12)) {
    i.file<-sprintf("%s/ICOADS3/IMMA/IMMA1_R3.0.0_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),year,month)
    if(!file.exists(i.file)) next
    obs<-ReadObs(i.file)
    ids<-unique(obs$ID)
    for(f in ids) {
       if(is.null(bias.ship[[f]])) next
       w<-which(obs$ID==f & !is.na(obs$SLP))
       if(length(w)==0) next
       obs$SLP[w]<-obs$SLP[w]-bias.ship[[f]]
       obs$SUPD[w]<-sprintf("%s Ship SLP bias=%5.2f",obs$SUPD[w],bias.ship[[f]])
     }
    for(deck in unique(obs$DCK)) {
        if(length(bias.year[[year]])<deck) next
        if(is.null(bias.year[[year]][[deck]])) next
        w<-which(!(obs$ID %in% names(bias.ship)) & !is.na(obs$SLP) & obs$DCK==deck)
        if(length(w)>0) {
           obs$SLP[w]<-obs$SLP[w]-bias.year[[year]][[deck]]
           obs$SUPD[w]<-sprintf("%s deck/year SLP bias=%5.2f",obs$SUPD[w],bias.year[[year]][[deck]])
        }
    }
    o.file<-gzfile(sprintf("%s/icoads_3.0_SLP_debiased/IMMA1_R3.0.0_SLPD_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),year,month),open='w')
    WriteObs(obs,o.file)
    close(o.file)
  }
}
