# Plot systematic comparisons of observed SLP and reanalysis climatology
#  across the 19th century.

library(IMMA)
library(grid)

diffs<-numeric(0)
filter<-numeric(0)
yr<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
    yr<-c(yr,obs$YR)
  }
}
w<-which(!is.na(diffs))
d2<-diffs[w]
y2<-yr[w]
bias.year<-list()
for(y in seq(1800,1870)) {
    w<-which(y2==y)
    bias.year[[y]]<-mean(d2[w],na.rm=TRUE)
}
saveRDS(bias.year,'bias.year.Rdata')

