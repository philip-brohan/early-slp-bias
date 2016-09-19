# Plot systematic comparisons of observed SLP and reanalysis climatology
#  between ships, in deck 721
# Filter also by pressure correction

library(IMMA)
library(grid)


diffs.u<-numeric(0)
filter.u<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month))
    w<-which(obs$DCK==721 & obs$PB==2)
    if(length(w)==0) next
    obs<-obs[w,]
    diffs.u<-c(diffs.u,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter.u<-c(filter.u,obs$ID)
  }
}

diffs.c<-numeric(0)
filter.c<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month))
    w<-which(obs$DCK==721 & obs$PB==1)
    if(length(w)==0) next
    obs<-obs[w,]
    diffs.c<-c(diffs.c,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter.c<-c(filter.c,obs$ID)
  }
}

filters<-unique(c(filter.c,filter.u))

pdf(file="Bias_by_ship_by_PB.pdf",
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
	       gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
               w<-which(filter.u==filters[f])
               if(length(w)==0) next
               bias.mean<-mean(diffs.u[w],na.rm=TRUE)
               bias.se<-sd(diffs.u[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(f,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
	       gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)
               w<-which(filter.c==filters[f])
               if(length(w)==0) next
               bias.mean<-mean(diffs.c[w],na.rm=TRUE)
               bias.se<-sd(diffs.c[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(f,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()
