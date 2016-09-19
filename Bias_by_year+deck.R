# Plot systematic comparisons of observed SLP and reanalysis climatology
#  between decks, 1800-1870

library(IMMA)
library(grid)


diffs<-numeric(0)
filter<-numeric(0)
yr<-numeric(0)
for(year in seq(1800,1870)) {
  for(month in seq(1,12)) {
    obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month))
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
    filter<-c(filter,obs$DCK)
    yr<-c(yr,obs$YR)
  }
}

filters<-unique(filter)

pdf(file="Bias_by_year+deck.pdf",
    width=10,height=20,family='Helvetica',
    paper='special',pointsize=18,bg='white')

  for(f in c(248)) {  # First one with x-axis
    pushViewport(viewport(width=1.0,height=0.27,x=0.0,y=0.0,
			  just=c("left","bottom"),name="Page",clip='on'))
       pushViewport(plotViewport(margins=c(4,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(-15,15),clip='off'))
	     grid.xaxis(at=seq(1800,1870,10),main=T)
	     grid.text('Date',y=unit(-3,'lines'))
	     grid.yaxis(main=T)
	     grid.text(f,x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year)
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
  y<-0.27
  for(f in c(701,721,192,780)) { # rest smaller, no x-axis
    pushViewport(viewport(width=1.0,height=0.18,x=0.0,y=y,
			  just=c("left","bottom"),name="Page",clip='on'))
       y<-y+0.18
       pushViewport(plotViewport(margins=c(0,6,0,0)))
	  pushViewport(dataViewport(seq(1800,1870),c(-15,15),clip='off'))
	     grid.yaxis(main=T)
	     grid.text(f,x=unit(-4,'lines'),rot=90)

             # horizontal line at 0
	     gp=gpar(col=rgb(0.5,0.5,0.5,1),fill=rgb(0.5,0.5,0.5,1),lwd=1)
             grid.lines(x=unit(c(1800,1870),'native'),
                        y=unit(0,'native'),gp=gp)

	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)
             if(f==701 || f==192) gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)

             for(year in seq(1800,1870)) {
               w<-which(filter==f & yr==year)
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

dev.off()
