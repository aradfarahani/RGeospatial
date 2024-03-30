#Mapping Points shp file and attributes
#load data
library(rgdal)
library(sp)
library(sf);library(GISTools); library(tmap)

library(readxl)
climatology <- read_excel("F:/without external/R/climatology.xls")
View(climatology)

head(climatology)
climatology$OBJECTID_1<- NULL
head(climatology)
summary(climatology)
summary(climatology$dama)

#define coordinate 
coords.tmp<- cbind(climatology$x,climatology$y)
View(coords.tmp)
colnames(coords.tmp)<- c("long","lat")
View(coords.tmp)

#create the spatial point shp file
climatology.sp <- SpatialPointsDataFrame(coords.tmp,data = data.frame(climatology),
                                         proj4string = CRS("+proj=longlat"))
class(climatology.sp)
head(climatology)

#convert to sf
climatology_sf <- st_as_sf(climatology.sp)
class(climatology_sf)
View(climatology_sf)
plot(climatology.sp)
plot(climatology_sf[,5])

#plot point map in ostan polygon
plot(ostan)
plot(climatology.sp,add=T,col="red",pch=20)

#tmap
tm_shape(ostan)+ tm_fill("lightgreen")+
  tm_shape(climatology_sf)+ tm_dots(size=0.5,alpha=.3,col = "red")

# plot by size
library(grid)
p1<-tm_shape(ostan)+ tm_fill("lightgreen")+
  tm_shape(climatology_sf)+ tm_bubbles("baresh",shape = 19,scale = 1,
                                       alpha=0.6,title.size="Rain (mm)")
#plot by color
p2<-tm_shape(ostan)+ tm_fill("lightgreen")+
  tm_shape(climatology_sf)+ tm_dots("baresh",shape = 19,size = 0.6,palette="PuBuGn",
                                       alpha=0.8,title.size="Rain (mm)")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

print(p1,vp=viewport(layout.pos.col = 1,height = 5))
print(p2,vp=viewport(layout.pos.col = 2,height = 5))

tmap_arrange(p1,p2)

#specify data subset to plot
summary(climatology$baresh)

index_cl <- climatology_sf$baresh >333.1
summary(index_cl)

rain <- climatology_sf[index_cl,]
head(rain)
View(rain)

#plot the subset
tm_shape(ostan)+tm_fill("lightblue")+tm_borders()+
  tm_shape(rain)+ tm_dots(col=brewer.pal(5,"Reds"),shape = 19,
                          alpha=0.5,size = 1)+
  tm_layout(title = "rain>333.1 mm", title.position = c("left","bottom"))

tm_shape(ostan)+tm_fill("lightblue")+tm_borders()+
  tm_shape(rain)+ tm_dots(col=brewer.pal(5,"Reds")[5],shape = 19,
                          alpha=0.5,size = 1)+
  tm_layout(title = "rain>333.1 mm", title.position = c("left","bottom"))

#add text to point map
summary(climatology$dama)
index_dama <- climatology_sf$dama>25
summary(index_dama)

tmpdama<- climatology_sf[index_dama,]
View(tmpdama)

#create plot point map with text
tm_shape(ostan)+tm_fill("lightgreen")+tm_borders()+
  tm_shape(tmpdama)+tm_dots(col = brewer.pal(5,"Reds")[4],
                            shape = 19,alpha=0.5,size = 1)+
  tm_text("name1",size="dama",scale=1,root=4,size.lowerbound=0.6,
          bg.color="white",bg.alpha = 0.75,auto.placement = 1, 
          legend.size.show = F)+
  tm_layout(title = "Temperature>25 C", title.position = c("right","top"))

#Mapping polyline and attribute
#create a clip area
par(mar=c(1.5,1.5,1.5,1.5))
plot(roads)
library(raster)
cropbox_road<- drawExtent()
#crop road
roadcrop <- crop(roads,cropbox_road)
plot(roadcrop,col="blue")

#convert to sf data
roadcrop_sf <- st_as_sf(roadcrop)
View(roadcrop_sf)
names(roadcrop_sf)
plot(roadcrop_sf[,5])
#tmap plot based on type road
tm_shape (roadcrop_sf)+tm_lines ("Road_Type", palette = "Dark2",
                                 title.col ="Roads Type by color")+
  tm_legend(outside=T)

#based on color
tm_shape (roadcrop_sf)+tm_lines ("Shape_Leng", palette = "Dark2",
                                 title.col ="Roads Length by color")+
  tm_legend(outside=T)

#based on Width
tm_shape (roadcrop_sf)+tm_lines (lwd="Shape_Leng",scale = 5, palette = "Dark2",
                                 legend.lwd.show = T,
                                 title.lwd = "Roads Length by Width")+
  tm_legend(outside=T)

#based on together
tm_shape (roadcrop_sf)+tm_lines ("Shape_Leng",lwd="Shape_Leng",scale = 7, 
                                 palette = "Dark2",
                                 legend.lwd.show = T,
                                 title.lwd = "Roads Length by Width",
                                 title.col = "Roads Length By Color")+
  tm_legend(outside=T)

#make map with plot function
library(RColorBrewer)
brewer.pal(7,"Dark2")
col<- c(brewer.pal(7,"Dark2"))[roadcrop$Road_Type]
head(col)
#plot
plot(roadcrop,col=col,
     lwd=3,main="Iran Roads type")
legend("bottomleft",legend = levels(roadcrop$Road_Type),
       fill = brewer.pal(7,"Dark2"))
#add scale an compass
scalebar(100,xy=click(),type="bar",divs = 4,below = "100 km")
compassRose(49,39.5,cex=0.5)
locator()




