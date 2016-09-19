# Plot systematic comparisons of observed SLP and reanalysis climatology
#  across the 19th century.

library(IMMA)
library(grid)

diffs<-numeric(0)
filter<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter<-c(filter,obs$ID)
  }
}
t<-table(filter)
w<-which(t>10)
filters<-attr(t,'dimnames')$filter[w]
bias.ship<-list()
bias.ship[unique(filter)]<-0
for(f in seq_along(filters)) {
    w<-which(filter==filters[f])
    bias.ship[[filters[f]]]<-mean(diffs[w],na.rm=TRUE)
}
bias.ship[[' ']]<-0 # For missing IDs
saveRDS(bias.ship,'bias.ship.Rdata')

bias.mean<-rep(NA,100)
bias.se<-rep(NA,100)

for(year in seq(1800,1870)) {
  obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,1))
  w<-which(is.na(obs$ID))
  if(length(w)>0) obs$ID[w]<-' '
  obs$SLP<-obs$SLP-unlist(bias.ship[obs$ID])
  diffs<-obs$SLP- obs$TWCR.prmsl.norm/100
  for(month in seq(2,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    w<-which(is.na(obs$ID))
    if(length(w)>0) obs$ID[w]<-' '
    obs$SLP<-obs$SLP-unlist(bias.ship[obs$ID])
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
  }
  bias.mean[year-1799]<-mean(diffs,na.rm=TRUE)
  bias.se[year-1799]<-sd(diffs,na.rm=TRUE)/
                        sqrt(length(diffs))
}
  
pdf(file="Bias_by_year_corrected_by_ship.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(1800,1900),c(-10,10),clip='off'))
	     grid.xaxis(at=seq(1800,1900,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1899),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)

             for(year in seq(1800,1899)) {
               grid.lines(x=unit(year,'native'),
                          y=unit(c(bias.mean[year-1799]+2*bias.se[year-1799],
                                   bias.mean[year-1799]-2*bias.se[year-1799]),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()
