# Get length of time used to make adjustment.

library(IMMA)
library(grid)
library(lubridate)
library(lattice)

diffs<-numeric(0)
filter<-numeric(0)
dte<-NULL
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm)
    filter<-c(filter,sprintf("%s%d",obs$ID,obs$SID))
    if(is.null(dte)) {
      dte<-ymd(sprintf("%04d-%02d-%02d",obs$YR,obs$MO,as.integer(obs$HR)))
    } else {
       dte<-c(dte,ymd(sprintf("%04d-%02d-%02d",obs$YR,obs$MO,as.integer(obs$HR))))
     }
  }
}
w<-which(!is.na(diffs))
d2<-diffs[w]
dte<-dte[w]
f2<-filter[w]
t<-table(f2)
w<-which(t>10)
filters<-attr(t,'dimnames')$f2[w]
bias.ship<-list()
#bias.ship[unique(filter)]<-0
for(f in seq_along(filters)) {
    w<-which(filter==filters[f])
    bias.ship[[filters[f]]]<-as.numeric(max(dte[w],na.rm=TRUE)-
                                        min(dte[w],na.rm=TRUE))
}

pdf('correction_period.pdf')
histogram(unlist(bias.ship)/365,breaks=60,xlim=c(-1,30),xlab='Years of operation')
dev.off()

