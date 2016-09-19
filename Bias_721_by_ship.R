# Plot systematic comparisons of observed SLP and reanalysis climatology
#  between ships, in deck 721

library(IMMA)
library(grid)

diffs<-numeric(0)
filter<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    w<-which(obs$DCK==721)
    if(length(w)==0) next
    obs<-obs[w,]
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter<-c(filter,obs$ID)
  }
}

filters<-unique(filter)

pdf(file="Bias_by_ship.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(seq_along(filters),c(-30,30),clip='off'))
	     grid.xaxis(at=seq_along(filters),label=rep('',length(filters)),main=T)
	     grid.text('Ship',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(seq_along(filters),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)

             for(f in seq_along(filters)) {
               w<-which(filter==filters[f])
               bias.mean<-mean(diffs[w],na.rm=TRUE)
               bias.se<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(f,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()
