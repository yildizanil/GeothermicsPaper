#################################################################################
#              Ground heat exchange potential of Green Infrastructure           #
#                         Yildiz, A. and Stirling, R.A.                         #
#      This script plots Fig. 3 of the manuscript Yildiz & Stirling (2021)      #
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
hydro.link <- "https://ndownloader.figshare.com/files/26387719?private_link=0c9aab5b3ab9471b4252"
water.flux.link <- "https://ndownloader.figshare.com/files/26388181?private_link=dbd3135fc6d61ea3e212"
thermal.cond.link <- "https://ndownloader.figshare.com/files/26388400?private_link=a6ea892798672e5494fb"
hydro <- read.csv(hydro.link,stringsAsFactors=F)
water.flux <- read.csv(water.flux.link,stringsAsFactors=F)
thermal.cond <- read.csv(thermal.cond.link,stringsAsFactors=F)
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
pdf(paste0(file.loc,"FIG_Hydro.pdf"),height=130/25.4,width=150/25.4)

layout(matrix(c(1,2,2,rep(3,5),c(4,5,6,7,8,9,10,11),12,13,13,rep(14,5)),nrow=8,ncol=3),
       heights=c(5,30,5,20,20,20,20,10),widths=c(5,140,5))

par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(las=0)
mtext("Water flux [mm]",side=2,line=-1)
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(las=0)
mtext("Volumetric water content [%]",side=2,line=-1)

par(mar=c(0,1.25,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Rainfall","Potential evapotranspiration"),col=c(1),pt.bg=c(8,green),ncol=2,bty="n",adj=c(0,0.5),pch=c(22,22))

par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(-10,40),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=-10,x1=seq(startdate,enddate,60*60*24),y1=40,col="gray87",lty=1)
segments(x0=startdate,y0=seq(-10,40,5),x1=enddate,y1=seq(-10,40,5),col="gray87",lty=1)
axis(1,tck=0.02,at=axis_ticks2,labels=NA)
axis(1,tck=0.04,at=axis_ticks1,labels=NA)
axis(2,tck=0.02)   
box()
for(i in 1:nrow(water.flux))
{
   rect(xleft=as.POSIXct(water.flux$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(water.flux$Time[i+1],"UTC"),ytop=water.flux$Rainfall[i],border=T,col=8)  
   rect(xleft=as.POSIXct(water.flux$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(water.flux$Time[i+1],"UTC"),ytop=-1*water.flux$PET[i],border=T,col=green)  
}
par(las=0)
text(startdate,40,"(a)",adj=c(0,1))

par(mar=c(0,1.25,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
legend("center",c("Volumetric water content","Thermal conductivity"),col=c(blue,1),ncol=2,bty="n",adj=c(0,0.5),lwd=2,lty=c(1,3))

index <- c(5,7,11,15)

for(i in 1:4)
{
   par(mar=c(0.25,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,xlim=c(startdate,enddate),ylim=c(0,30),type="l",axes=F,xlab=NA,ylab=NA)
   segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=30,col="gray87")
   segments(x0=startdate,y0=seq(0,30,5),x1=enddate,y1=seq(0,30,5),col="gray87")
   axis(1,tck=0.02,at=axis_ticks2,labels=NA)
   axis(1,tck=0.04,at=axis_ticks1,labels=NA)
   axis(2,tck=0.02,at=c(0,10,20,30),labels=c(0,10,20,30))
   box()
   lines(hydro[,index[1]]~as.POSIXct(hydro$Time,tz="UTC"),lwd=2,col=blue)
   text(startdate,30,"(b)",adj=c(0,1))
   par(new=T)
   plot(1,1,xlim=c(startdate,enddate),ylim=c(0.4,1.6),type="l",axes=F,xlab=NA,ylab=NA)
   axis(4,tck=0.02,at=seq(0.4,1.6,0.4))
   lines(thermal.cond[,6-i]~as.POSIXct(thermal.cond$Time,tz="UTC"),lwd=3,col=1,lty=3)
   if(i==4)
   {
      mtext("Time [dd-mm-2019]",side=1,line=1)
      axis(1,tck=0.04,at=axis_ticks1,labels=axis_names)
   }
}

par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(las=0)
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,pch="",xlab=NA,ylab=NA,axes=F,xlim=c(0,1),ylim=c(0,1))
par(las=0)
mtext("Thermal conductivity [W/mK]",side=2,line=-1)

dev.off()