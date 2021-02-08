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
enddate <- as.POSIXct("2019-08-06 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#                            Importing datasets                                 
#-------------------------------------------------------------------------------#
soil.temp.link <- "https://ndownloader.figshare.com/files/26389138?private_link=17cf9f87af8a160f14c2"
meteo.link <- "https://ndownloader.figshare.com/files/26386726?private_link=31e846de1eb5f79ac815"
heat.flux.link <- "https://ndownloader.figshare.com/files/26389165?private_link=5f8ecef47b99ce475574"

soil.temp <- read.csv(soil.temp.link,header = T,stringsAsFactors = F)
meteo <- read.csv(meteo.link,stringsAsFactors=F)
heat.flux <- read.csv(heat.flux.link,stringsAsFactors=F)

soil.temp.set1 <- soil.temp[which(as.POSIXct(soil.temp[,1],"UTC") > startdate &
                                   as.POSIXct(soil.temp[,1],"UTC") < enddate),]
meteo.set1 <- meteo[which(as.POSIXct(meteo[,1],"UTC") > startdate &
                                     as.POSIXct(meteo[,1],"UTC") < enddate),]
heat.flux.set1 <- heat.flux[which(as.POSIXct(heat.flux[,1],"UTC") > startdate &
                                     as.POSIXct(heat.flux[,1],"UTC") < enddate),]

dates <- read.csv("https://raw.githubusercontent.com/yildizanil/GeothermicsPaper/main/fieldtest.csv?token=AIINA47D27ALIQAN63L37HLAEGR72")
meteo.midday <- (as.POSIXct(meteo[,1],"UTC")+60*60*12)
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
#                    Assigning indices for different datasets                            
#-------------------------------------------------------------------------------#
index.pch <- c(NA,18,NA,17,0,1,15,16)
index.lty <- c(3,3,1,1,1,1,1,1)
index.col <- c(green,blue,green,blue,green,blue,green,blue)
index.name <- c("@ 100 mm","@ 250 mm","@ 350 mm","@ 450 mm","@ 550 mm","@ 650 mm","@ 750 mm","@ 850 mm")
index.column <- c(4,6,7,8,9,10,11,13)
#-------------------------------------------------------------------------------#
# Defining file location
#-------------------------------------------------------------------------------#
file.loc <- "C:/Users/Anil/Desktop/Geothermics/Figures/PDF/"
#-------------------------------------------------------------------------------#
# Plotting a pdf 
#-------------------------------------------------------------------------------#
pdf(paste0(file.loc,"FIG_SetI.pdf"),height=125/25.4,width=150/25.4)

layout(matrix(c(1,2,3,4),4,1),heights=c(5,10,70,40),widths=c(150))

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(15,30),type="l",axes=F,xlab=NA,ylab=NA)
legend("center",c("Mean air temperature"),pch=10,col=1,bty="n")

par(mar=c(0,2.25,0,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(15,30),type="l",axes=F,xlab=NA,ylab=NA)
legend("center",index.name,pch=index.pch,lty=index.lty,col=index.col,bty="n",ncol=4,lwd=2)

par(mar=c(2.25,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(15,30),type="l",axes=F,xlab=NA,ylab=NA)
for(i in 1:3)
{
   rect(xleft=as.POSIXct(dates[i,1],"UTC"),ybottom=15,
        xright=as.POSIXct(dates[i,2],"UTC"),ytop=30,border=NA,col="gray90")
}
segments(x0=seq(startdate,enddate,60*60*24),y0=15,x1=seq(startdate,enddate,60*60*24),y1=30,col="gray87")
segments(x0=startdate,y0=seq(15,30,2.5),x1=enddate,y1=seq(15,30,2.5),col="gray87")
axis(1,tck=0.02,at=axis_ticks2,labels=NA)
axis(1,tck=0.04,at=axis_ticks1,labels=axis_names)
axis(2,tck=0.02,at=c(15,20,25,30),labels=c(15,20,25,30))
box()

for(i in 1:length(index.column))
{
   points(soil.temp.set1[seq(1,nrow(soil.temp.set1),50),index.column[i]]~as.POSIXct(soil.temp.set1[seq(1,nrow(soil.temp.set1),50),1],"UTC"),
          col=index.col[i],pch=index.pch[i]) 
   lines(soil.temp.set1[,index.column[i]]~as.POSIXct(soil.temp.set1[,1],"UTC"),lwd=2,col=index.col[i],lty=index.lty[i])
}
points(meteo$AirTemp_Mean[1:18]~meteo.midday[1:18],col=1,pch=10)

par(las=0)
mtext("Time [dd-mm-2019]",side=1,line=1.25)
mtext("Air/soil temperature [°C]",side=2,line=1)
for(i in 1:3)
{
   text(as.POSIXct(dates[i,1],"UTC")+60*60*36,30,paste0("Test I-",i),adj=c(0.5,1))
   text(as.POSIXct(dates[i,2],"UTC")+60*60*48,30,paste0("Off"),adj=c(0.5,1))
}
text(startdate,30,"(a)",adj=c(0,1))

par(mar=c(2.25,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(-5,35),type="l",axes=F,xlab=NA,ylab=NA)
for(i in 1:3)
{
   rect(xleft=as.POSIXct(dates[i,1],"UTC"),ybottom=-5,
        xright=as.POSIXct(dates[i,2],"UTC"),ytop=35,border=NA,col="gray90")
}
segments(x0=seq(startdate,enddate,60*60*24),y0=-5,x1=seq(startdate,enddate,60*60*24),y1=35,col="gray87")
segments(x0=startdate,y0=seq(-5,35,5),x1=enddate,y1=seq(-5,35,5),col="gray87")
axis(1,tck=0.02,at=axis_ticks2,labels=NA)
axis(1,tck=0.04,at=axis_ticks1,labels=axis_names)
axis(2,tck=0.02)
box()

lines(heat.flux$heat.flux.bottom~as.POSIXct(heat.flux[,1],"UTC"),lwd=2)

par(las=0)
mtext(expression(paste("Heat flux @ 940 mm [W/m"^2,"]")),side=2,line=1)
for(i in 1:3)
{
   text(as.POSIXct(dates[i,1],"UTC")+60*60*36,35,paste0("Test I-",i),adj=c(0.5,1))
   text(as.POSIXct(dates[i,2],"UTC")+60*60*48,35,paste0("Off"),adj=c(0.5,1))
}
mtext("Time [dd-mm-2019]",side=1,line=1.25)
text(startdate,35,"(b)",adj=c(0,1))

dev.off()