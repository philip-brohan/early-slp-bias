# Plot systematic comparisons of observed SLP and reanalysis climatology
#  across the 19th century.

library(IMMA)
library(grid)

bias.mean<-rep(NA,100)
bias.se<-rep(NA,100)

for(year in seq(1800,1899)) {
  obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,1))
  diffs<-obs$SLP- obs$TWCR.prmsl.norm/100
  for(month in seq(2,12)) {
    obs<-readRDS(sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month))
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm/100)
  }
  bias.mean[year-1799]<-mean(diffs,na.rm=TRUE)
  bias.se[year-1799]<-sd(diffs,na.rm=TRUE)/
                        sqrt(length(diffs))
}

  
pdf(file="Bias_by_year.pdf",
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
