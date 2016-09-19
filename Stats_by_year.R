# Plot systematic comparisons of observed SLP with and without adjustments

library(IMMA)
library(grid)

old<-list()
new<-list()
for(year in seq(1800,1900)) {
  old[[year]]<-list()
  new[[year]]<-list()
  for(month in seq(1,12)) {
    old[[year]][[month]]<-list()
    new[[year]][[month]]<-list()
    of<-sprintf("/scratch/hadpb/icoads_3.0/IMMA1_R3.0.0_%04d-%02d.gz",year,month)
    if(!file.exists(of)) next
    o<-ReadObs(of)
    old[[year]][[month]]$count<-length(which(!is.na(o$SLP)))
    old[[year]][[month]]$mean<-mean(o$SLP,na.rm=TRUE)
    old[[year]][[month]]$sd<-sd(o$SLP,na.rm=TRUE)    
    old[[year]][[month]]$median<-median(o$SLP,na.rm=TRUE)
    old[[year]][[month]]$mad<-mad(o$SLP,na.rm=TRUE)    
    nf<-sprintf("/scratch/hadpb/icoads_3.0_SLP_debiased/IMMA1_R3.0.0_SLPD_%04d-%02d.gz",year,month)
    if(file.exists(nf)) {
      o<-ReadObs(nf)
      new[[year]][[month]]$count<-length(which(!is.na(o$SLP)))
      new[[year]][[month]]$mean<-mean(o$SLP,na.rm=TRUE)
      new[[year]][[month]]$sd<-sd(o$SLP,na.rm=TRUE)
      new[[year]][[month]]$median<-median(o$SLP,na.rm=TRUE)
      new[[year]][[month]]$mad<-mad(o$SLP,na.rm=TRUE)    
    }
  }
}
  
pdf(file="Stats_by_year.pdf",
    width=10*sqrt(2),height=10,family='Helvetica',
    paper='special',pointsize=18,bg='white')

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(1800,1900),c(0,11),clip='off'))
	     grid.xaxis(at=seq(1800,1900,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T,at=log(c(10,100,1000,10000)),
                               label=c('10','100','1000','1000'))
	     grid.text('No. of obs/month',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     #gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             #grid.lines(x=unit(c(1800,1899),'native'),
             #           y=unit(0,'native'),gp=gp)

	     gp.new=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1))
	     gp.old=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1))

             for(year in seq(1800,1899)) {
                for(month in seq(1,12)) {
                  if(is.null(old[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(log(old[[year]][[month]]$count),'native'),
                                pch=21,
                                size=unit(0.01,'npc'),
                                gp=gp.old)
                   if(is.null(new[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(log(new[[year]][[month]]$count),'native'),
                                pch=21,
                                size=unit(0.007,'npc'),
                                gp=gp.new)
                 }
             }

	  popViewport()
       popViewport()
    popViewport()

grid.newpage()

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(1800,1900),c(995,1025),clip='off'))
	     grid.xaxis(at=seq(1800,1900,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Mean SLP (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     #gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             #grid.lines(x=unit(c(1800,1899),'native'),
             #           y=unit(0,'native'),gp=gp)

	     gp.new=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1))
	     gp.old=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1))

             for(year in seq(1800,1899)) {
                for(month in seq(1,12)) {
                  if(is.null(old[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(old[[year]][[month]]$mean,'native'),
                                pch=21,
                                size=unit(0.01,'npc'),
                                gp=gp.old)
                   if(is.null(new[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(new[[year]][[month]]$mean,'native'),
                                pch=21,
                                size=unit(0.007,'npc'),
                                gp=gp.new)
                 }
             }

	  popViewport()
       popViewport()
    popViewport()

grid.newpage()

    pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='off'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(c(1800,1900),c(995,1025),clip='off'))
	     grid.xaxis(at=seq(1800,1900,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text('Median SLP (hPa)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     #gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             #grid.lines(x=unit(c(1800,1899),'native'),
             #           y=unit(0,'native'),gp=gp)

	     gp.new=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1))
	     gp.old=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1))

             for(year in seq(1800,1899)) {
                for(month in seq(1,12)) {
                  if(is.null(old[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(old[[year]][[month]]$median,'native'),
                                pch=21,
                                size=unit(0.01,'npc'),
                                gp=gp.old)
                   if(is.null(new[[year]][[month]]$count)) next
                     grid.points(x=unit(year+month/12,'native'),
                                y=unit(new[[year]][[month]]$median,'native'),
                                pch=21,
                                size=unit(0.007,'npc'),
                                gp=gp.new)
                 }
             }

	  popViewport()
       popViewport()
    popViewport()

dev.off()
