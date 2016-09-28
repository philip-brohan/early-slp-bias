# Plot systematic comparisons of observed SLP and reanalysis climatology

library(IMMA)
library(grid)
library(lubridate)

diffs<-numeric(0)
filter<-numeric(0)
dt<-NULL
for(year in seq(1850,1859)) {
  for(month in seq(1,12)) {
    rdf<-sprintf("/scratch/hadpb/icoads_bias_check/%04d.%02d.Rdata",year,month)
    if(!file.exists(rdf)) next
    obs<-readRDS(rdf)
    diffs<-c(diffs,obs$SLP-obs$TWCR.prmsl.norm)
    filter<-c(filter,obs$ID)
    w<-which(is.na(obs$HR))
    obs$HR[w]<-12
    if(is.null(dt)) {
      dt<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",obs$YR,obs$MO,obs$DY,obs$HR))
    } else {
      dt<-c(dt,ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",obs$YR,obs$MO,obs$DY,obs$HR)))
    }
  }
}
w<-which(!is.na(diffs))
d2<-diffs[w]
dt2<-dt[w]
f2<-filter[w]
t<-table(f2)
w<-which(t>1000)
filters<-attr(t,'dimnames')$f2[w]
bias.ship<-list()
#bias.ship[unique(filter)]<-0

pdf(file="lm_by_ship_1850s.pdf",
    width=20*sqrt(2),height=20,family='Helvetica',
    paper='special',pointsize=12,bg='white')

xh<-as.integer(sqrt(length(filters)/sqrt(2)))
yh<-as.integer(length(filters)/xh)+1
for(f in seq_along(filters)) {
    if(is.na(filters[f])) next
    w<-which(f2==filters[f])
    bias.ship[[filters[f]]]<-lm(d2[w]~dt2[w])
    pushViewport(viewport(width=1/yh,height=1/xh,y=((f-1)%%xh)/xh,x=as.integer((f-1)/xh)/yh,
			  just=c("left","bottom"),name="Page",clip='off'))
       plot.1.ship(dt2[w],d2[w],bias.ship[[filters[f]]])
    popViewport(0)
    
}
dev.off()
#saveRDS(bias.ship,'lm.ship.Rdata')

plot.1.ship<-function(dt,d,lm) {
  
       pushViewport(plotViewport(margins=c(4,4,0,0)))
	  pushViewport(dataViewport(dt,d,clip='off'))
             xt<-pretty(dt)
	     grid.xaxis(main=T,at=xt,label=as.character(xt))
	     grid.yaxis(main=T)
             grid.points(x=unit(dt,'native'),
                         y=unit(d,'native'),
                         pch=20,
                         size=unit(0.02,'npc'))
             # red horizontal line at mean
	     gp=gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=2)
             m<-mean(d,na.rm=TRUE)
             grid.lines(x=unit(dt,'native'),
                        y=unit(m,'native'),gp=gp)
             # blue line with linear fit
	     gp=gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=2)
             grid.lines(x=unit(dt,'native'),
                        y=unit(predict(lm),'native'),gp=gp)
       
	  popViewport()
       popViewport()
}
