#!/usr/bin/Rscript --no-save

# Add 20CR SLP normal and sd for each ob

library(parallel)
library(lubridate)
library(IMMA)
library(GSDF.TWCR)
library(getopt)

opt = getopt(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer"
));
if ( is.null(opt$year  ) )   { stop("Year not specified") }

year<-opt$year
month<-opt$month
n.total<-days_in_month(ymd(sprintf("%04d-%02d-01",year,month)))*24


# load the R3 obs
o<-ReadObs(sprintf("%s/icoads_3.0/IMMA1_R3.0.0_%04d-%02d.gz",
                   Sys.getenv('SCRATCH'),year,month))
# Add the SLP nomal and sd columns
TWCR.prmsl.norm<-rep(NA,length(o$YR))
o<-cbind(o,TWCR.prmsl.norm)
TWCR.prmsl.sd<-rep(NA,length(o$YR))
o<-cbind(o,TWCR.prmsl.sd)

# Add the TWCR data for a selected hour
compare<-function(n.hour) {
  day<-as.integer(n.hour/24)+1
  hour<-n.hour%%24
  w<-which(!is.na(o$DY) & !is.na(o$HR) &
           !is.na(o$LAT) & !is.na(o$LON) &
           o$DY==day & as.integer(o$HR)==hour)
  if(length(w)==0) return(NULL)
  p.norm<-TWCR.get.slice.at.hour('prmsl',2014,month,day,hour,
                                 type='normal',version='3.4.1')
  p.sd<-TWCR.get.slice.at.hour('prmsl',2014,month,day,hour,
                                 type='standard.deviation',version='3.4.1')
  o$TWCR.prmsl.norm[w]<<-GSDF.interpolate.ll(p.norm,o$LAT[w],o$LON[w])/100
  o$TWCR.prmsl.sd[w]<<-GSDF.interpolate.ll(p.sd,o$LAT[w],o$LON[w])/100
  return(length(w))
}
  

res<-lapply(seq(0,n.total),compare)
saveRDS(o,sprintf("%s/icoads_bias_check/%04d.%02d.Rdata",
                   Sys.getenv('SCRATCH'),year,month))
