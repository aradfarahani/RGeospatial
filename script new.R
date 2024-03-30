#factor function
#a vector variable
Texture.Class <- c("Clay","Loamy","Loamy","Siltloam","Loamy","Sandy","Sandy")
Texture.Class
str(Texture.Class)
Texture.Class<- factor(Texture.Class)
Texture.Class
str(Texture.Class)

nutrientsoil<- factor(c("High","High","Low","Low","Low","Medium","Low"),levels = c("Low",
                                                                                   "Medium","High"))
nutrientsoil
nutrientsoil>"Low"
nutrientsoil<- ordered(c("High","High","Low","Low","Low","Medium","Low"),levels = c("Low",
                                                                                   "Medium","High"))
nutrientsoil
nutrientsoil>"Low"
sort(nutrientsoil)
sort(Texture.Class)

#Basic Plot
#make point Plot
x1 <- rnorm(100)
x1
y1<- rnorm(100)
y1
plot(x1,y1)

plot(x1,y1,pch=5,col="red")
plot(x1,y1,pch=20,col="red")

#make Line plot
x2<- seq(0,2*pi,len=100)
x2
y2=sin(x2)
y2
plot(x2,y2,type = "l")
plot(x2,y2,type = "l",lwd=5,col="darkgreen")

#change tolerance Of Y
plot(x2,y2,type = "l",col="darkgreen", lwd=5,ylim = c(-1.2,1.2))
#plot point around line
y2r <- y2 + rnorm(100,0,0.1)
points(x2,y2r,pch=16,col="darkred")

#make other line plot
y4 = cos (x2)
y4
plot(x2,y2,type = "l",lwd=3,col="darkgreen")
#add cos line
lines(x2,y4,lwd=3,col="darkblue",lty=2)

#plot of 1 row and 2 columns
par(mfrow=c(1,2))
plot(x2,y2,type = "l",lwd=3,col="darkgreen")
plot(x2,y2,type = "l",lwd=3,col="darkgreen",ylim = c(-1.2,1.2))
points(x2,y2r,pch=20,col="darkred")
par(mfrow=c(1,1))

#install.packages("GISTools",dep=T)
library(GISTools)
data(georgia)
View (georgia.polys)
appling <- georgia.polys[[1]]
head(appling)
tail(appling)
str(appling)
plot(appling,asp=1,type="n",xlab="Easting",ylab="Northing")
#add ploygon appling
polygon (appling,density = 14,angle = 15)
#or
plot(appling,asp=1,type="n",xlab="Easting",ylab="Northing")
#add ploygon appling
polygon (appling,density = 30,angle = 90)

plot(appling,asp=1,type="n",xlab="Easting",ylab="Northing")
#make point map
points(x=runif(500,126,132)*10000,y=runif(500,103,108)*10000,pch=16,col="red")
#add polygon
polygon(appling,col=rgb(0,0.5,0.7,0.4))

#add text
plot(appling,asp=1,type="n",xlab="Easting",ylab="Northing")
#add polygon
polygon(appling,col=rgb(0,0.5,0.7,0.2))
#add text
text(1287000,1053000,"Appling County",cex = 1.5)
text(1287000,1049000,"Georgia",col = "darkred")

#show list of color
colors()

#add text
plot(appling,asp=1,type="n",xlab="Easting",ylab="Northing")
#add polygon
polygon(appling,col="tomato2")
#add text
text(1287000,1053000,"Appling County",cex = 1.5,col="skyblue4")
text(1287000,1049000,"Georgia",col = "yellowgreen")

#plotting raster data
data(meuse.grid)

#make a grid data
grd=SpatialPixelsDataFrame(points = meuse.grid[c("x","y")],data=meuse.grid)
head(grd)
class(grd)
str(grd)

#change plot parameters
par(mfrow=c(1,2))
par(mar=c(0,0,0,0))
#plot raster or grid data
image (grd, "dist")

#install.packages("RColorBrewer",dep=T)
library(RColorBrewer)
#make a color palette with 7 classes
greenpal = brewer.pal(7,"Greens")
#make this for plot grid
image (grd,"dist",col=greenpal)

par(mfrow=c(1,1))

contour(grd,"dist")

#using ggplot2 for plotting
#install.packages("ggplot2",dep=T)
library(ggplot2)
qplot(x2,y2r,col=I("darkred"),ylim = c(-1.2,1.2))+
  #make line map
  geom_line(aes(x2,y2),col=I("darkgreen"),size=I(1.5))+
  theme(axis.text = element_text(size = 20))

#bold axis
qplot(x2,y2r,col=I("darkred"),ylim = c(-1.2,1.2))+
  #make line map
  geom_line(aes(x2,y2),col=I("darkgreen"),size=I(1.5))+
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 20,face = "bold"))

class(appling)
appling= data.frame(appling)
colnames(appling)= c("X","Y")
str(appling)

#creat polygon with qplot
qplot(X,Y,data=appling,geom = "polygon",asp=1,color=I("black"),fill=I(rgb(0,0.5,0.7,0.4)))+
  theme(axis.text = element_text(size=12),axis.title = element_text(size = 20))

qplot(X,Y,data=appling,geom = "point",asp=1,color=I("black"),fill=I(rgb(0,0.5,0.7,0.4)))+
  theme(axis.text = element_text(size=12),axis.title = element_text(size = 20))

#make df for point map
df<- data.frame (x=runif(500,126,132)*10000,y=runif(500,103,108)*10000)
#use ggplot
ggplot(appling,aes(x=X,y=Y))+geom_polygon(fill=I(rgb(0,0.5,0.7,0.4)))+
  geom_point(data=df,aes(x,y),col=I("red"))+coord_fixed()+
  theme(axis.text = element_text(size=12),axis.title = element_text(size = 20))

getwd ()
#change work folder
setwd("F:/without external/R/data save")
getwd ()

#save data frame
write.csv(appling,file="appling.csv")
write.csv(appling,file="appling.csv",row.names = F)

ls()

save(list = ls(),file="mydata.RData")
save(list = "appling",file="mydata2.RData")
save(list =c("appling","df","grd") ,file="mydata3.RData")

remove(appling)

#read export csv data
applingnew <-read.csv("appling.csv")

load("mydata2.RData")
