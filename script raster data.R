#install.packages("raster",dep=T)
library(raster);library(rgdal); library(sp);library(GISTools)
#load raster data
DEM<-raster("F:\\without external\\R\\Demsistan.tif")
#View raster attributes
DEM
setMinMax(DEM)
cellStats(DEM,min)
cellStats(DEM,max)
cellStats(DEM,range)
DEM@crs
DEM@extent
hist(DEM,main="Distibution of elevation values",
     col="purple",maxpixels=22000000)
#plot raster data
plot(DEM, main="DEM, Sistan Plain")
image(DEM)

#plot the spesified the range of values
image(DEM,zlim=c(400,600))

col=terrain.colors(5)
image(DEM,zlim=c(400,600),col=col,main="DEM Sistan Plain")
brk<-c(420,700,950,1200,1550,2000)
image(DEM,breaks=brk,col=col,main="DEM Sistan Plain")
#add custom legend
plot(DEM,breaks=brk,col=col,main="DEM Sistan Plain",legend=F)
legend("bottomleft",
       legend = c("lowest","a bit higher","middle ground",
                  "higher yet", "highest"),
       fill = col,bty="n")

#plot raster with reverse Legend
plot(DEM,breaks=brk,col=col,main="DEM Sistan Plain",legend=F)
legend("bottomleft",
       legend = c("Highest", "Higher yet", "Middle ground",
                  "A bit higher", "Lowest"),cex = 0.7,
       fill =rev(col) ,bty="n",title = "Elevation",title.col = "lightblue")

col2=rainbow(5)
plot(DEM,breaks=brk,col=col2,main="DEM Sistan Plain",legend=F)
legend("bottomleft",
       legend = c("Highest", "Higher yet", "Middle ground",
                  "A bit higher", "Lowest"),cex = 0.7,
       fill =rev(col) ,bty="n",title = "Elevation",title.col = "blue")
library(RColorBrewer)
display.brewer.all()
col3=brewer.pal(5,"BrBG")
plot(DEM,breaks=brk,col=col3,main="DEM Sistan Plain",legend=F)
legend("bottomleft",
       legend = c("Highest", "Higher yet", "Middle ground",
                  "A bit higher", "Lowest"),cex = 0.7,
       fill =rev(col) ,bty="n",title = "Elevation",title.col = "blue")
#multiply each pixel un the raster by 2
dem<- DEM*2
dem
DEM
plot(dem,main="DEM Sistan with all VAlues Doubled")

#crop Raster
plot(DEM)
#define extent
cropbox1<- drawExtent()
# make a raster based cropbox
DEMCROP1<- crop(DEM,cropbox1)
DEMCROP1
plot(DEMCROP1)

#plot 3d raster
library(rgl)
library(rasterVis)
plot3D(DEM)
plot3D(DEM,col=rainbow)
plot3D(DEM,col=col3)

#plot high and low ground
plot(DEM)
highground <- DEM>1200
class(highground)
plot(highground)
lowground<- DEM<1200
class(lowground)
plot(lowground)

#change coordinate
crs(DEM)
zone41.pr<- "+proj=utm +zone=41 +north +ellps=WGS84 +datum=WGS84 +units=m
+no_defs"

projected_DEM <- projectRaster(DEM,crs = zone41.pr)
projected_DEM
DEM
plot(projected_DEM)


#make slope & aspect and hill map
slope<-terrain(projected_DEM,opt="slope",unit="degrees",neighbors=4)
plot(slope)

sloperad<-terrain(projected_DEM,opt="slope",unit="radians",neighbors=4)
plot(sloperad)

aspect<- terrain(projected_DEM,opt="aspect",unit="degrees")
plot(aspect)

#make hillshade map
slope<-terrain(projected_DEM,opt="slope",unit="radians")
aspect<- terrain(projected_DEM,opt="aspect",unit="radians")
hill<-hillShade(slope = slope,aspect = aspect,angle=45, direction = 315)
plot(hill,col=gray.colors(20,start = 0,end = 1))

#plot raster file by tmap package
library(tmap)
tm_shape(hill)+
        tm_raster(palette = grey(0:10/10),style = "cont",legend.show = F)

#plot DEM
tm_shape(DEM)+
        tm_raster(alpha = 0.5,palette = terrain.colors(10),style = "cont",
                  title = "Elevation (m)",legend.show = T)

#combine both raster
tm_shape(hill)+
        tm_raster(palette = grey(0:10/10),style = "cont",legend.show = F)+
#plot DEM
tm_shape(DEM)+
        tm_raster(alpha = 0.5,palette = terrain.colors(10),style = "cont",
                  title = "Elevation (m)",legend.show = T)

flowdir<-terrain(projected_DEM,opt="flowdir")
plot(flowdir)

x<-terrain(projected_DEM,opt=c("slope","aspect"),"degrees")
plot(x)

windows()
plot(projected_DEM,col=rainbow(25,alpha = 0.4))
windows()
plot(slope,col=rainbow(25,alpha = 0.8))
windows()
plot(aspect,col=rainbow(25,alpha = 0.7))

#stack Layers
compareRaster(projected_DEM,slope,aspect,hill,flowdir)

rasterstack<-stack(projected_DEM,slope,aspect,hill,flowdir)
names(rasterstack)
plot(rasterstack)

names(rasterstack)[4]<-paste("hill")
names(rasterstack)
plot(rasterstack$hill)
#plot by tmap
tmap_mode("plot")
tm_shape(rasterstack)+tm_raster(col=c("slope","Demsistan"),
                                title = c("Slope","Elevation"),palette = "Set3")+
        tm_layout(legend.position = c("left","bottom"))
      
tmap_mode("view")
tm_shape(rasterstack)+
        tm_raster(col = "flowdir",title = "Flowdirection", 
                  breaks = c(seq(0.140,20)))+
        tm_layout(legend.format = list(digits=1),
                  legend.position =c("left","bottom") )

#save raster 
writeRaster(DEM,filename = "DEM2sistan.tif",format="GTiff")

demnew<- raster(choose.files())
plot(demnew)










