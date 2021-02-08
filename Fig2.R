#################################################################################
#              Ground heat exchange potential of Green Infrastructure           #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 2 of the manuscript Yildiz & Stirling (2021)      #
#                      written by Anil Yildiz, Dr. sc.                          #
#################################################################################
#-------------------------------------------------------------------------------#
#                    Time frame presented in the paper                          
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-09-12 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#                            Importing datasets                                 
#-------------------------------------------------------------------------------#
meteo_link <- "https://ndownloader.figshare.com/files/26386726?private_link=31e846de1eb5f79ac815"
meteo <- read.csv(meteo_link,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
#                         Axis tick marks and labels                            
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq.Date(as.Date(startdate),as.Date(enddate),"week"))
axis_names <- paste0(substr(axis_seq,start=9,stop=10),"-",substr(axis_seq,start=6,stop=7))
axis_ticks1 <- as.POSIXct(axis_seq,"UTC")
axis_ticks2 <- seq(startdate,enddate,60*60*24)
#-------------------------------------------------------------------------------#
# Defining file location
#-------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Desktop/Geothermics/Figures/PDF/"
#-------------------------------------------------------------------------------#
# Plotting a pdf 
#-------------------------------------------------------------------------------#
pdf(paste0(file.loc,"FIG_Meteo.pdf"),height=85/25.4,width=150/25.4)

layout(matrix(c(1,2,3,4),nrow=4,ncol=1),heights=c(10,35,35,5))

par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlab=NA,ylab=NA,axes=F,pch="")
legend("center",c("Rainfall","Relative Humidity","Air temperature","Net radiation"),
       bty="n",ncol=2,
       pch=c(15,NA,NA,NA),lwd=c(NA,2,2,2),
       lty=c(NA,2,1,3),col=c(8,blue,2,1),adj=c(0,0.5),xjust=0.5)

par(mar=c(1,2.5,0.25,2.5),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,50),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,
         x1=seq(startdate,enddate,60*60*24),y1=50,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,50,10),
         x1=enddate,y1=seq(0,50,10),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_ticks2,labels=NA)
axis(1,tck=0.04,at=axis_ticks1,labels=axis_names)
axis(2,tck=0.02)     
for(i in 1:nrow(meteo))
{
   rect(xleft=as.POSIXct(meteo$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(meteo$Time[i+1],"UTC"),ytop=meteo$Rain[i],border=T,col=8)  
}
text(startdate,50,"(a)",adj=c(0,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(0,30),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(0,30,6),labels=seq(0,30,6))
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$AirTemp_Max,rev(meteo$AirTemp_Min)),
        border=NA,col=rgb(1,0,0,0.2))
lines(meteo$AirTemp_Mean~as.POSIXct(meteo$Time,"UTC"),col=2,lwd=2)
par(las=0)
box()
mtext("Daily rainfall [mm]",side=2,line=1.5)
mtext("Air temperature [°C]",side=4,line=1.5)

par(mar=c(1,2.5,0.25,2.5),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,100),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,
         x1=seq(startdate,enddate,60*60*24),y1=100,col="gray87",lty=1)
segments(x0=startdate,y0=seq(0,100,20),
         x1=enddate,y1=seq(0,100,20),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_ticks2,labels=NA)
axis(1,tck=0.04,at=axis_ticks1,labels=axis_names)
axis(2,tck=0.02)
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$Humid_Max,rev(meteo$Humid_Min)),
        border=NA,col=rgb(32/256,137/256,203/256,0.2))
lines(meteo$Humid_Mean~as.POSIXct(meteo$Time,"UTC"),col=blue,lwd=2,lty=2)
text(startdate,100,"(b)",adj=c(0,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(-100,400),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(-100,400,100))
lines(meteo$NetRad~as.POSIXct(meteo$Time,"UTC"),col=1,lwd=3,lty=3)
par(las=0)
par(las=0)
mtext("Time [dd-mm-2019]",side=1,line=1)
mtext("Relative humidity [%]",side=2,line=1.5)
mtext(expression(paste("Net radiation [W/m"^2,"]")),side=4,line=1.5)
box()

dev.off()