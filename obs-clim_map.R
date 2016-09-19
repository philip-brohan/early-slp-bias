#!/usr/bin/Rscript --no-save

# Obs pressure anomalies w.r.t climatology (3.0)

library(GSDF)
library(GSDF.WeatherMap)
library(GSDF.TWCR)
library(lubridate)
library(IMMA)
library(getopt)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer"
));
if ( is.null(opt$year ) )   { stop("Year not specified") }
if ( is.null(opt$mont ) )   { stop("Month not specified") }
if ( is.null(opt$day  ) )   { stop("Day not specified") }

Imagedir<-sprintf("%s/images/icoads_3.0_pressure_anomalies_debiased",
                  Sys.getenv('SCRATCH'))
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'show.mslp',F)
Options<-WeatherMap.set.option(Options,'show.ice',T)
Options<-WeatherMap.set.option(Options,'show.obs',T)
Options<-WeatherMap.set.option(Options,'show.fog',F)
Options<-WeatherMap.set.option(Options,'show.wind',F)
Options<-WeatherMap.set.option(Options,'show.temperature',F)
Options<-WeatherMap.set.option(Options,'show.precipitation',F)
Options<-WeatherMap.set.option(Options,'temperature.range',12)
Options<-WeatherMap.set.option(Options,'obs.size',1.5)
Options<-WeatherMap.set.option(Options,'ice.colour',Options$land.colour)
                               
Options<-WeatherMap.set.option(Options,'obs.colour',rgb(255,215,0,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'lat.min',-90)
Options<-WeatherMap.set.option(Options,'lat.max',90)
Options<-WeatherMap.set.option(Options,'lon.min',-180)
Options<-WeatherMap.set.option(Options,'lon.max',180)
Options<-WeatherMap.set.option(Options,'pole.lon',180)
Options<-WeatherMap.set.option(Options,'pole.lat',90)

Options$mslp.lwd<-1
Options$mslp.base=0                         # Base value for anomalies
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.crange=500                     # Anomaly for max contour colour
Options$mslp.step=100                       # Smaller -more contours
Options$mslp.tpscale=1                      # Smaller -contours less transparent

obs.cache<-list()
ReadObs.cache<-function(file.name,start,end) {
  result<-NULL
  if(!is.null(obs.cache[[file.name]])) {
    result<-obs.cache[[file.name]]
  } else {
     if(length(names(obs.cache))>2) {
       obs.cache<-list()
       gc(verbose=FALSE)
     }
     obs.cache[[file.name]]<-ReadObs(file.name)
    result<-obs.cache[[file.name]]
  }
  w<-which(is.na(result$HR))
  if(length(w)>0) result$HR[w]<-12
  result.dates<-ymd_hms(sprintf("%04d-%02d-%02d %02d:%02d:00",
                                as.integer(result$YR),
                                as.integer(result$MO),
                                as.integer(result$DY),
                                as.integer(result$HR),
                                as.integer((result$HR%%1)*60)))
  w<-which(result.dates>=start & result.dates<end)
  result<-result[w,]  
  return(result)
}
# Get observations from ICOADS
ICOADS.3.0.get.obs<-function(year,month,day,hour,duration) {
  start<-ymd_hms(sprintf("%04d-%02d-%02d %02d:30:00",year,month,day,hour))-
    hours(duration/2)
  end<-start+hours(duration)
  files<-unique(c(sprintf("%s/icoads_3.0_SLP_debiased/IMMA1_R3.0.0_SLPD_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),as.integer(year(start)),
                                as.integer(month(start))),
                  sprintf("%s/icoads_3.0_SLP_debiased/IMMA1_R3.0.0_SLPD_%04d-%02d.gz",
                        Sys.getenv('SCRATCH'),as.integer(year(end)),
                                as.integer(month(end)))))
  result<-data.frame()
  for(file in files) {
    o<-ReadObs.cache(file,start,end)
    if(length(colnames(result))==0) {
      result<-o
    } else {
      cols <- intersect(colnames(result), colnames(o))
      result<-rbind(result[,cols], o[,cols])
    }
  }
  w<-which(result$LON>180)
  if(length(w)>0) result$LON[w]<- result$LON[w]-360

  # Add the pressure normals
  p.norm<-TWCR.get.slice.at.hour('prmsl',2014,month,day,hour,
                                 type='normal',version='3.4.1')
  w<-which(!is.na(result$DY) & !is.na(result$HR) & !is.na(result$SLP) &
           !is.na(result$LAT) & !is.na(result$LON))
  TWCR.prmsl.norm<-rep(NA,length(result$YR))
  TWCR.prmsl.norm[w]<-GSDF.interpolate.ll(p.norm,result$LAT[w],result$LON[w])
  result<-cbind(result,TWCR.prmsl.norm)
  
  return(result)
}

obs.get.colour<-function(slp) {
   value<-max(-0.999,min(0.999,(slp-Options$mslp.base)/
              Options$mslp.crange))
   value<-value/2+0.5
   return(Options$wind.palette[ceiling(value*length(Options$wind.palette))])
}

Draw.obs.pressure<-function(obs,Options) {

  min.pressure<-Options$mslp.base-Options$mslp.crange
  w<-which(obs$SLP<min.pressure)
  if(length(w)>0) {
    Options$obs.colour<-obs.get.colour(min.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
  for(mp in seq(Options$mslp.base-Options$mslp.crange+Options$mslp.step,
                Options$mslp.base+Options$mslp.crange,Options$mslp.step)) {
    w<-which(obs$SLP<mp & obs$SLP>=mp-Options$mslp.step)
    if(length(w)>0) {
      Options$obs.colour<-obs.get.colour(mp-Options$mslp.step/2)
      WeatherMap.draw.obs(obs[w,],Options)
    }
 }
  max.pressure<-Options$mslp.base+Options$mslp.crange
  w<-which(obs$SLP>max.pressure)
  if(length(w)>0) {
    Options$obs.colour<-obs.get.colour(max.pressure)
    WeatherMap.draw.obs(obs[w,],Options)
  }
}


land<-WeatherMap.get.land(Options)

plot.day<-function(year,month,day) {    

    hour<-12

    image.name<-sprintf("%04d-%02d-%02d:%02d.png",year,month,day,hour)

    ifile.name<-sprintf("%s/%s",Imagedir,image.name)
    #if(file.exists(ifile.name) && file.info(ifile.name)$size>0) return()

    obs.ic<-ICOADS.3.0.get.obs(year,month,day,hour,144)

     png(ifile.name,
             width=1080*WeatherMap.aspect(Options),
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    Options$label<-sprintf("%04d-%02d-%02d",year,month,day)
          pushViewport(dataViewport(c(Options$lon.min,Options$lon.max),
                                    c(Options$lat.min,Options$lat.max),
                                    extension=0))
          WeatherMap.draw.land(NULL,Options)
    
           obs.ic$SLP<-obs.ic$SLP*100-obs.ic$TWCR.prmsl.norm
           obs.ic$Longitude<-obs.ic$LON
           obs.ic$Latitude<-obs.ic$LAT
           Draw.obs.pressure(obs.ic,Options)
          
           WeatherMap.draw.label(Options)
          upViewport()
    dev.off()
    gc(verbose=FALSE)
}

plot.day(opt$year,opt$month,opt$day)
