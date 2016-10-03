# Plot systematic comparisons of observed SLP and reanalysis climatology
#  1800-1870. Distinguish between correctable, no-ID, and rare-ID ships.

library(IMMA)
library(grid)
bias.ship<-readRDS('bias.ship.Rdata')

diffs<-numeric(0)
cdiffs<-numeric(0)
filter<-numeric(0)
yr<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm)
    ftmp<-obs$DCK
    w<-which(is.na(obs$ID))
    if(length(w)>0) ftmp[w]<-1
    ids<-unique(obs$ID)
    cd<-obs$SLP-obs$TWCR.prmsl.norm
    for(f in ids) {
       if(is.na(f)) next
       w<-which(obs$ID==f)
       if(length(w)==0) next
       if(!is.null(bias.ship[[f]])) {
          ftmp[w]<-3
          cd[w]<-cd[w]-bias.ship[[f]]
       } else {
          ftmp[w]<-2
       }
    }
    cdiffs<-c(cdiffs,cd)
    filter<-c(filter,ftmp)
    yr<-c(yr,obs$YR)
  }
}

filters<-unique(filter)
titles<-c('No ID','Rare ID (<10 obs)','Correctable')

pdf(file="Bias_by_year+correctable.pdf",
    width=20/sqrt(2),height=20,family='Helvetica',
    paper='special',pointsize=18,bg='white')

  for(f in c(1)) {  # First one with x-axis
    pushViewport(viewport(width=1.0,height=0.3,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='on'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(-15,15),clip='off'))
	     grid.xaxis(at=seq(1800,1870,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text(sprintf("Mean bias: %s",titles[f]),x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)
	     gp2=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year)
               cd.bias.mean<-mean(cdiffs[w],na.rm=TRUE)
               cd.bias.se<-sd(cdiffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(cd.bias.mean+2*cd.bias.se,
                                   cd.bias.mean-2*cd.bias.se),'native'),
                          gp=gp2)
               bias.mean<-mean(diffs[w],na.rm=TRUE)
               bias.se<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
  }
  y<-0.3
  for(f in c(2,3)) { # rest smaller, no x-axis
    pushViewport(viewport(width=1.0,height=0.23,x=0.0,y=y,
			  just=c("left","bottom"),name="Page",clip='on'))
       y<-y+0.23
       pushViewport(plotViewport(margins=c(0,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(-15,15),clip='off'))
	     grid.yaxis(main=T)
	     grid.text(sprintf("Mean bias: %s",titles[f]),x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)
	     gp2=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year)
               cd.bias.mean<-mean(cdiffs[w],na.rm=TRUE)
               cd.bias.se<-sd(cdiffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(cd.bias.mean+2*cd.bias.se,
                                   cd.bias.mean-2*cd.bias.se),'native'),
                          gp=gp2)
               bias.mean<-mean(diffs[w],na.rm=TRUE)
               bias.se<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
  }

# All obs
    pushViewport(viewport(width=1.0,height=0.23,x=0.0,y=y,
			  just=c("left","bottom"),name="Page",clip='on'))
       y<-y+0.23
       pushViewport(plotViewport(margins=c(0,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(-15,15),clip='off'))
	     grid.yaxis(main=T)
	     grid.text('Mean bias: All obs',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)
	     gp2=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(yr==year)
               cd.bias.mean<-mean(cdiffs[w],na.rm=TRUE)
               cd.bias.se<-sd(cdiffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(cd.bias.mean+2*cd.bias.se,
                                   cd.bias.mean-2*cd.bias.se),'native'),
                          gp=gp2)
               bias.mean<-mean(diffs[w],na.rm=TRUE)
               bias.se<-sd(diffs[w],na.rm=TRUE)/sqrt(length(w))
               grid.lines(x=unit(year,'native'),
                          y=unit(c(bias.mean+2*bias.se,
                                   bias.mean-2*bias.se),'native'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()


# Graph the number of obs in each category as well
grid.newpage()

  for(f in c(1)) {  # First one with x-axis
    pushViewport(viewport(width=1.0,height=0.3,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='on'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(0,1),clip='off'))
	     grid.xaxis(at=seq(1800,1870,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text(sprintf("Fraction: %s",titles[f]),x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)
	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year & !is.na(diffs))
               w2<-which(yr==year & !is.na(diffs))
               grid.points(x=unit(year,'native'),
                          y=unit(length(w)/length(w2),'native'),
                          pch=21,size=unit(0.01,'npc'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
  }
  y<-0.3
  for(f in c(2,3)) { # rest smaller, no x-axis
    pushViewport(viewport(width=1.0,height=0.23,x=0.0,y=y,
			  just=c("left","bottom"),name="Page",clip='on'))
       y<-y+0.23
       pushViewport(plotViewport(margins=c(0,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(0,1),clip='off'))
	     grid.yaxis(main=T)
	     grid.text(sprintf("Fraction: %s",titles[f]),x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)
	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year & !is.na(diffs))
               w2<-which(yr==year & !is.na(diffs))
               grid.points(x=unit(year,'native'),
                          y=unit(length(w)/length(w2),'native'),
                          pch=21,size=unit(0.01,'npc'),
                          gp=gp)
             }

	  popViewport()
       popViewport()
    popViewport()
  }

# All obs
    pushViewport(viewport(width=1.0,height=0.23,x=0.0,y=y,
			  just=c("left","bottom"),name="Page",clip='on'))
       y<-y+0.23
       pushViewport(plotViewport(margins=c(0,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(0,200000),clip='off'))
	     #grid.yaxis(main=T,at=log(c(1,10,100,1000,10000,100000)),label=c('1','10','100','1,000','10,000','100,000'))
	     grid.yaxis(main=T)
	     grid.text('All obs (count)',x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0,0,0,1),fill=rgb(0,0,0,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)
	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=6)

             for(year in seq(1800,1870)) {
               w<-which(yr==year & !is.na(diffs))
               grid.points(x=unit(year,'native'),
                          y=unit(length(w),'native'),
                          pch=21,size=unit(0.01,'npc'),
                          gp=gp)
              }

	  popViewport()
       popViewport()
    popViewport()

dev.off()
