###########################################################################################
##This code reads in deployment data for chamber flux measurements and calculates CO2 and CH4 fluxes,
##requires environmental data in the read file, and all LGR files in the directory in their native format
#JTC, July 8, 2014
getfluxed <- function(metadata="LGRmetadata_MissR_April2015_newinstrument.csv", output.file="fluxtest.csv"){
#####################################################################################
  #LGR Deployment data
  metadata$start1=as.POSIXct(metadata$start1)
  metadata$stop1=as.POSIXct(metadata$stop1)
  metadata$start2=as.POSIXct(metadata$start2)
  metadata$stop2=as.POSIXct(metadata$stop2)
  metadata$start3=as.POSIXct(metadata$start3)
  metadata$stop3=as.POSIXct(metadata$stop3)
  str(metadata)
##############################################################################################################
  flux.co2.calc=rep(NA, nrow(metadata))
  flux.ch4.calc=rep(NA, nrow(metadata))
  GTV.co2.calc=rep(NA, nrow(metadata))
  GTV.ch4.calc=rep(NA, nrow(metadata))
  mean.r2.co2=rep(NA, nrow(metadata))
  mean.r2.ch4=rep(NA, nrow(metadata))
  Sc.CO2=rep(NA, nrow(metadata))
  Sc.CH4=rep(NA, nrow(metadata))
  K600.co2=rep(NA, nrow(metadata))
  K600.ch4=rep(NA, nrow(metadata))
for(i in 1:nrow(metadata)){
  #set environmental parameters
  AirTemp= 273.15+metadata$AirTemp_C[i];#Kelvin
  Pressure= 0.000986923266716*metadata$Pressure_hPa[i]#atmospheres
  GasConstant= 44.617516*Pressure; #corrected gas constant in mol/m3
  ChamberHt=  metadata$Chamber_Ht_m[i]
  SurfaceCO2=metadata$LGR_CO2_ppm[i] #ppmv
  SurfaceCH4=metadata$LGR_CH4_ppm[i] #ppmv
  WaterTemp=metadata$WaterTemp_C[i]#Celsius
  AtmosphereCO2=metadata$AtmosphereCO2_ppm[i]#ppmv
  AtmosphereCH4=1.89#metadata$AtmCH4_LGR[i]#ppmv
  #read file from LGR
  site.name=metadata$Site[i]
  file.name=metadata$LGR.file[i]
  laser=read.csv(file.name,skip=1) #,comment.char="-", 
  laser$Time=as.POSIXct(laser$Time, tz="America/Chicago",  format="%m/%d/%Y %H:%M:%S")-60*2
  laser=laser[!duplicated(laser[,"Time"]),]#remove duplicated timestamps at the second level
  laser$second=seq(from=1, to=nrow(laser), by=1)# add a string of seconds to the dataset
  str(laser)
  ################select flux replicates by timestamp
  start1=metadata$start1[i]
  stop1=metadata$stop1[i]
  time1=subset(laser, Time > as.POSIXct(start1) & Time <= as.POSIXct(stop1))
  ##########  
  start2=metadata$start2[i]
  stop2=metadata$stop2[i]
  time2=subset(laser, Time > as.POSIXct(start2) & Time <= as.POSIXct(stop2))
  ##########
  start3=metadata$start3[i]
  stop3=metadata$stop3[i]
  time3=subset(laser, Time > as.POSIXct(start3) & Time <= as.POSIXct(stop3))
  ###########################################################################
  #plot/model the slopes
  dev.new()
  pdf(file=paste("Site",site.name, file.name,".pdf", sep="_"), height=5, width=5)
  par(mfrow=c(2,3),mar=c(4,4,1,1) + 0.1,oma=c(4,3,1,1))
  plot(time1$second, time1$X.CO2.d_ppm)
  lm1=lm(time1$X.CO2.d_ppm~time1$second)
  summary(lm1)
  abline(lm1, col="red", lwd=2)
  slope1=NA
  slope1=summary(lm1)[[4]][[2]]
  r1=NA
  r1 = summary(lm1)$r.squared
  ##
  plot(time2$second, time2$X.CO2.d_ppm)
  lm2=lm(time2$X.CO2.d_ppm~time2$second)
  summary(lm2)
  abline(lm2, col="red", lwd=2)
  slope2=NA
  slope2=summary(lm2)[[4]][[2]]
  r2=NA
  r2 = summary(lm2)$r.squared
  ##
  plot(time3$second, time3$X.CO2.d_ppm)
  lm3=lm(time3$X.CO2.d_ppm~time3$second)
  summary(lm3)
  abline(lm3, col="red", lwd=2)
  slope3=NA
  slope3=summary(lm3)[[4]][[2]]
  r3=NA
  r3 = summary(lm3)$r.squared
  r2.co2=c(r1, r2, r3)
  mean.r2.co2[i]=mean(na.omit(r2.co2))
  slopes.co2=c(slope1, slope2, slope3)
  slopemean.co2=mean(na.omit(slopes.co2))#
  ############################################
  #repeat plotting/model of slopes for CH4
  plot(time1$second, time1$X.CH4.d_ppm)
  lm1b=lm(time1$X.CH4.d_ppm~time1$second)
  summary(lm1b)
  abline(lm1b, col="red", lwd=2)
  slope1b=NA
  slope1b=summary(lm1b)[[4]][[2]]
  r1b=NA
  r1b = summary(lm1b)$r.squared
  ##
  plot(time2$second, time2$X.CH4.d_ppm)
  lm2b=lm(time2$X.CH4.d_ppm~time2$second)
  summary(lm2b)
  abline(lm2b, col="red", lwd=2)
  slope2b=NA
  slope2b=summary(lm2b)[[4]][[2]]
  r2b=NA
  r2b = summary(lm2b)$r.squared
  ##
  plot(time3$second, time3$X.CH4.d_ppm)
  lm3b=lm(time3$X.CH4.d_ppm~time3$second)
  summary(lm3b)
  abline(lm3b, col="red", lwd=2)
  slope3b=NA
  slope3b=summary(lm3b)[[4]][[2]]
  r3b=NA
  r3b = summary(lm3b)$r.squared
  slopes.ch4=c(slope1b, slope2b, slope3b)
  r2.ch4=c(r1b, r2b, r3b)
  mean.r2.ch4[i]=mean(na.omit(r2.ch4))
  slopemean.ch4=mean(na.omit(slopes.ch4))
  dev.off()
  #CO2 flux calculation, mol/m2/day
  source("CO2flux.r")
  flux.co2.calc[i]=signif(CO2flux(GasConstant, AirTemp, slopemean.co2, ChamberHt), digits=4)
  #CH4 flux calculation, mol/m2/day
  source("CH4flux.r")
  flux.ch4.calc[i]=signif(CH4flux(GasConstant, AirTemp, slopemean.ch4, ChamberHt), digits=4)
  #calculate gas transfer velocity for CO2
  source("GasTransfer_CO2.r")
  GTV.co2.calc[i]=signif(GTV_CO2(SurfaceCO2,AtmosphereCO2, Pressure, flux.co2.calc[i], WaterTemp), digits=4)
  GTV.ch4.calc[i]=signif(GTV_CH4(SurfaceCH4,AtmosphereCH4, Pressure, flux.ch4.calc[i], WaterTemp), digits=4)
}
###############################################################################################
############################################################################
  output = data.frame(metadata, flux.co2.calc, flux.ch4.calc, GTV.co2.calc, GTV.ch4.calc,mean.r2.co2, mean.r2.ch4)
  str(output)
  write.table(output, output.file, sep=",", row.names=FALSE, col.names=TRUE)
}