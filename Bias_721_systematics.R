# SLP bias in deck 721 - variation with latitude and temperature

library(IMMA)
library(grid)

diffs<-numeric(0)
filter<-numeric(0)
at<-numeric(0)
lat<-numeric(0)
lon<-numeric(0)
nslp<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month))
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter<-c(filter,obs$DCK)
    at<-c(at,obs$AT)
    lat<-c(lat,obs$LAT)
    lon<-c(lon,obs$LON)
    nslp<-c(nslp,obs$TWCR.prmsl.norm)
  }
}

diffs<-diffs[filter==721]
at<-at[filter==721]
lat<-lat[filter==721]
lon<-lon[filter==721]
nslp<-nslp[filter==721]
nslp<-nslp/100

t.x<-seq(1,40)
mn<-rep(NA,40)
se<-rep(NA,40)
for(t in seq_along(t.x)) {
   w<-which(at>t.x[t]-0.5 & at<t.x[t]+0.5)
   mn[t]<-mean(diffs[w],trim=0.25,na.rm=TRUE)
   se[t]<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
}

pdf(file="721_by_at.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(0,40),c(-50,50),clip='off'))
	     grid.xaxis(at=pretty(c(0,40)),main=T)
	     grid.text('AT (C)',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(seq(0,40),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=1)

              grid.points(x=unit(at,'native'),
                          y=unit(diffs,'native'),
                          size=unit(0.002,'npc'),
                          pch=21,
                          gp=gp)

              gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
             for(t in seq_along(t.x)) {
                grid.lines(x=unit(t.x[t],'native'),
                           y=unit(c(mn[t]+2*se[t],
                                    mn[t]-2*se[t]),'native'),
                           gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()

t.x<-seq(-60,60)
mn<-rep(NA,length(t.x))
se<-rep(NA,length(t.x))
for(t in seq_along(t.x)) {
   w<-which(lat>t.x[t]-0.5 & lat<t.x[t]+0.5)
   mn[t]<-mean(diffs[w],trim=0.25,na.rm=TRUE)
   se[t]<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
}
pdf(file="721_by_lat.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(-60,60),c(-50,50),clip='off'))
	     grid.xaxis(at=pretty(c(-60,60)),main=T)
	     grid.text('Latitude',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(seq(-60,60),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=1)

              grid.points(x=unit(lat,'native'),
                          y=unit(diffs,'native'),
                          size=unit(0.002,'npc'),
                          pch=21,
                          gp=gp)

              gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
             for(t in seq_along(t.x)) {
                grid.lines(x=unit(t.x[t],'native'),
                           y=unit(c(mn[t]+2*se[t],
                                    mn[t]-2*se[t]),'native'),
                           gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()

t.x<-seq(0,360)
mn<-rep(NA,length(t.x))
se<-rep(NA,length(t.x))
for(t in seq_along(t.x)) {
   w<-which(lon>t.x[t]-0.5 & lon<t.x[t]+0.5)
   mn[t]<-mean(diffs[w],trim=0.25,na.rm=TRUE)
   se[t]<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
}
pdf(file="721_by_lon.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(00,360),c(-50,50),clip='off'))
	     grid.xaxis(at=pretty(c(0,360)),main=T)
	     grid.text('Latitude',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(seq(0,360),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=1)

              grid.points(x=unit(lon,'native'),
                          y=unit(diffs,'native'),
                          size=unit(0.002,'npc'),
                          pch=21,
                          gp=gp)

              gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
             for(t in seq_along(t.x)) {
                grid.lines(x=unit(t.x[t],'native'),
                           y=unit(c(mn[t]+2*se[t],
                                    mn[t]-2*se[t]),'native'),
                           gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()
t.x<-seq(980,1035)
mn<-rep(NA,length(t.x))
se<-rep(NA,length(t.x))
for(t in seq_along(t.x)) {
   w<-which(nslp>t.x[t]-0.5 & nslp<t.x[t]+0.5)
   mn[t]<-mean(diffs[w],trim=0.25,na.rm=TRUE)
   se[t]<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
}
pdf(file="721_by_normal.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(980,1035),c(-50,50),clip='off'))
	     grid.xaxis(at=pretty(c(980,1035)),main=T)
	     grid.text('Normal',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Obs-normal (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(seq(980,1035),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=1)

              grid.points(x=unit(nslp,'native'),
                          y=unit(diffs,'native'),
                          size=unit(0.002,'npc'),
                          pch=21,
                          gp=gp)

              gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
             for(t in seq_along(t.x)) {
                grid.lines(x=unit(t.x[t],'native'),
                           y=unit(c(mn[t]+2*se[t],
                                    mn[t]-2*se[t]),'native'),
                           gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
dev.off()
