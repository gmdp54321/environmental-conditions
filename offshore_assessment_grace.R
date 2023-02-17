##############################################
# TOPEX
##############################################
# A string object with the name of the folder to read from
inputpath<-"C:/Users/Docencia/Downloads/01_topex-20230217/topex/"
#We read the first file corresponding to Jan 1993
#We create a string object with the name of the file we want to read
inputfile_sat<-paste(inputpath,"topex_19930101_19930201_data.txt",sep="")
sat1<-read.table(inputfile_sat,nrows=-1,header=TRUE,na.strings="-999.000")
sat1[248:250,]
#Significant wave height
#hws_sat:lon-lat-hws
hws_sat<-sat1[,c(1,2,3)]
#Backscatter coefficient
#s0_sat:lon-lat-s0
s0_sat<-sat1[,c(1,2,4)]
tz_sat<-as.data.frame((s0_sat*hws_sat*hws_sat)^0.25)
#Add two columns with lon -lat
#import from s0_sat
class(tz_sat)
tz_sat[,1:2]<-sat1[,1:2]
names(tz_sat)[3]<-"tz"
#Jan 1993
#We retrieve the names of the satellite files from the directory: command ‘list.files’
files<-list.files(path=inputpath)
length(files)

#We open a loop to read the rest of the monthly files
for (knt in seq(2,154,by=1)){
  #the name fo the file to open
  inputfile_satx<-paste(inputpath,files[knt],sep="")
  #We read the files
  satx<-read.table(inputfile_satx,header=TRUE,na.strings="-999.000")
  #We add a column to hws_sat:hws is in 3rd column
  hws_sat<-cbind(hws_sat,satx[,3])
  #We calculate the Tz (mean wave period) from sigma0
  #s0 is in 4th column
  #Mean wave period
  tz_sat0<-as.data.frame((satx[,4]*satx[,3]*satx[,3])^0.25)
  names(tz_sat0)[1]<-"tz"
  tz_sat<-cbind(tz_sat,tz_sat0)
  
  }
#We will calculate the wave energy flux at each gridpoint
pow_sat<-0.489*hws_sat*hws_sat*tz_sat
#We import the information on lon-lat from the first two columns of hws_sat
pow_sat[,1:2]<-hws_sat[,1:2]
#Calculate the average from column 3:156
pow_sat_aver<-rowMeans(pow_sat[,3:156],na.rm=TRUE)
pow_sat_aver
#Now we have to add the lon-lat columns
pow_sat_aver2<-cbind(pow_sat[,1:2],pow_sat_aver)
#Give names to the columns
names(pow_sat_aver2)[1]<-"lon"
names(pow_sat_aver2)[2]<-"lat"
names(pow_sat_aver2)[3]<-"wef"
#**** Graphical representation on a map *************
#We need the following packages
#Install
# install.packages("sp")
# install.packages("maptools")
# install.packages("maps")
# install.packages("mapdata")
# install.packages("rgdal")
# install.packages("shape")
#Use the packages
library("sp")
library("maps")
library("mapdata")
library("rgdal")
library("shape")
#Divide the drawing area into two
#Left parte:color scale
#Right part:map
layout(matrix(1:2,ncol=2),width=c(1,4),height=c(1,1))
#Create the palette we are going to use with 100 intermediate colors
coll<-colorRampPalette(c("purple","blue","lightblue","orange","yellow"))(100)
#See the maximum and minimun values of the WEF
max(pow_sat_aver2[,3],na.rm=TRUE)
min(pow_sat_aver2[,3],na.rm=TRUE)
#Max value of the WEF scale=105
#Min value of the WEF scale=0
#We draw the color scale in the left part of the image
colorlegend(zlim=c(0,105),zval=seq(0,105,by=10),
col=coll[1:100],main="kw/m",
posx=c(0.2,0.35),posy=c(0.05,0.9))
#We draw the map in the right part
map("worldHires",xlim=c(-10,-1),ylim=c(40,45),col="grey",fill=TRUE)
#map("worldHires",col="grey",fill=TRUE)
#We can add a box and axes
box();axis(1);axis(2)
#We can add a title and lables for the axes
title(main="TOPEX WEF [kW/m] Jan 1993-Oct 2005",
xlab="ºE",ylab="ºN")
#This regression relates wef and colors
rescalecolor<-1+(pow_sat_aver2[,3]*100/105)
#This puts a color point on its corresponding lon-lat
points(pow_sat_aver2[,1],pow_sat_aver2[,2] ,col=coll[rescalecolor])
#We can again overlay the land mask
map("worldHires",xlim=c(-10,-1),ylim=c(40,45),col="grey",fill=TRUE,add=TRUE)
#Save plot
outputpath<-inputpath
nameplot<-"TOPEX_wef"
#Save as png
plotfile<-paste(inputpath,nameplot,".png",sep="")
dev.copy (png, plotfile)
dev.off()
#Save as Encapsulated Postscript
setEPS()
plotfile<-paste(inputpath,nameplot,".eps",sep="")
dev.copy (postscript, plotfile,horizontal=F)
dev.off()
dev.off()
#save as PDF
plotfile2<-paste(outputpath,nameplot,".pdf",sep="")
dev.copy(pdf,plotfile2)
dev.off()

#Bay of Biscay
#If instead of gridpoints we want an interpolated contour surface
#we have to re-arrange data into a matrix where columns are increasing
#values of the longitude and rows are increasing values of the latitude
#To that purpose we have a library 'reshape2' and a command 'acast'
install.packages("reshape2")
library(reshape2)
#We create a mirror object
pow_sat_gp_mean2<-pow_sat_aver2
#We change the names of iyts columns to be "x","y" and "z"
names(pow_sat_gp_mean2)[1]<-"x"
names(pow_sat_gp_mean2)[2]<-"y"
names(pow_sat_gp_mean2)[3]<-"z"
names(pow_sat_gp_mean2)
#[1] "x" "y" "z"
#FIXME
#We apply the command 'acast' and store results in pow_sta_gp_mean3
pow_sat_gp_mean3<-acast(pow_sat_gp_mean2, x~y, value.var="z")
ncol(pow_sat_gp_mean3)
#[1] 91
nrow(pow_sat_gp_mean3)
#[1] 181
class(pow_sat_gp_mean3)
rownames(pow_sat_gp_mean3)
colnames(pow_sat_gp_mean3)
#In rows we have the lon and in columns the latitude
#The number of different values of longitude are in columns
#To plot this matrix as a contour.plot
zz<-pow_sat_gp_mean3
xx<-seq(-180,180,by=2)
yy<-seq(-90,90,by=2)
#A palette without a explicit number of colors
coll2<-colorRampPalette(c("purple","blue","lightblue","orange","yellow"))
#The satellite does not provide a good discrimination between land and sea
#so we can overlay the land mask with add=TRUE.
#To that purpose we need a function draw.map to overlay both graphics
#on the same axes
#coloured plot+maps
draw.map <- function() {maps::map("worldHires",xlim=c(-20,0),ylim=c(35,55), 
                                  fill=TRUE, col="grey", add=TRUE)}
draw.contour<-function(){contour(xx,yy,zz,nlevels=20,add=TRUE)}
filled.contour(xx,yy,zz,xlab="ºE", ylab="ºN",xlim=c(-20,0),
               ylim=c(35,55), color.palette=coll2,
               plot.axes={axis(1); axis(2) ;draw.contour();draw.map()},
               main="TOPEX. Wave energy [Kw/m]")
nameplot<-"TOPEX_wef_2"
#Save as png
plotfile<-paste(inputpath,nameplot,".png",sep="")
dev.copy (png, plotfile)
dev.off()
#Save as Encapsulated Postscript
setEPS()
plotfile<-paste(inputpath,nameplot,".eps",sep="")
dev.copy (postscript, plotfile,horizontal=F)
dev.off()
dev.off()
