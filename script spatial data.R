#read sp
#install.packages("sf",dep=T)
library(rgdal)
library(sp)
library(sf);library(GISTools)
ostan <-readOGR(dsn="F:\\without external\\R",layer = "OstanRevised")
ostan@data
ostan <-readOGR(dsn="F:\\without external\\R",layer = "OstanRevised",use_iconv = T,
                encoding = "UTF-8")
ostan@data

dfostan<- data.frame(ostan)
dfostan

city<-readOGR(dsn="F:\\without external\\R",layer ="City")
head(city)
city<-readOGR(dsn="F:\\without external\\R",layer ="City",use_iconv = T,
              encoding = "UTF-8")
head(city)

roads <- readOGR(dsn="F:\\without external\\R",layer ="roadsiran_pr")

ls()

class(ostan)
class(city)
class(roads)
#show attribute table of shp file
head(data.frame (ostan))
head(ostan@data)
head(city@data)
head(roads@data)
ostan@proj4string

#plot shp file
plot(ostan)
par(mar=c(1.5,1.5,1.5,1.5))

plot(roads,col="red")
plot(ostan,add=T)

plot(ostan)
plot(city,col="red",pch=20,add=T)

plot(ostan)
plot(city,col="red",pch=1,add=T)
plot(roads,col="blue",add=T)

plot(ostan,col="grey")
plot(city,col="red",pch=5,add=T)
plot(roads,col="blue",add=T,lwd=2)

plot(ostan,col="darkgreen",main="Iran")

plot(ostan,col="darkgreen")
title(main = "Iran provinces\n Area (Km sq)",
      cex.main=1,font.main=2,col.main="blue")
plot(city,col="red",pch=20,add=T)
plot(roads,col="blue",add=T,lwd=1)

colors ()

#using sf packages for analysis sp data
library(sf)
#convert sp to sf data
ostan_sf <-st_as_sf(ostan)
class(ostan_sf)
ostan_sf
View (ostan_sf)
str(ostan_sf)

plot(ostan_sf)
plot (ostan_sf[,5])
plot(ostan_sf[,c(4,6)])
#compare sf and sp 
head(ostan_sf)
head(data.frame(ostan))

#convert sf data to sp
g2 <- as(ostan_sf,"Spatial")
class(g2)
str(g2)
summary(g2)
plot(g2)

roads_sf<- st_as_sf(roads)
class(roads_sf)

city_sf<- st_as_sf(city)
class(city_sf)

plot(city_sf[,5])
plot(roads_sf[,6])

r2<- as(roads_sf,"Spatial")
class(r2)
plot(r2)

c2<- as(city_sf,"Spatial")
class(c2)
plot(c2)

#save or write sp data
writeOGR(obj = g2, dsn = ".",layer = "gosatn",driver = "ESRI Shapefile",
         overwrite_layer = T)
#save sp data with farsi font
writeOGR(obj = g2, dsn = ".",layer = "gosatn",driver = "ESRI Shapefile",
         overwrite_layer = T,layer_options = "ENCODING=UTF-8")
ostannew <- readOGR(dsn = ".",layer = "gosatn")
ostannew@data

writeOGR(obj = g2, dsn = ".",layer = "g2osatn",driver = "ESRI Shapefile",
         overwrite_layer = T,layer_options = "ENCODING=ISO-8859-1")
ostannew <- readOGR(dsn = ".",layer = "g2osatn")
ostannew@data

writeSpatialShape(ostan,fn="g3osatn")
ostannew <- readOGR(dsn = ".",layer = "g3osatn")
ostannew@data

st_write(ostan_sf,"g4ostan.shp",layer_options = "ENCODING=UTF-8",
         delete_layer = T)
ostannewSF <-readOGR(dsn = ".",layer = "g4ostan",use_iconv = T,
                     encoding = "UTF-8")
ostannewSF@data

getwd()
setwd ("F:/without external/R")
writeOGR(obj=roads,dsn = ".",layer="roads_iran",driver = "ESRI Shapefile",
         overwrite_layer = T)
newroadiran<- readOGR(dsn = ".",layer = "roads_iran")

writeOGR(obj=city,dsn = "F:/without external/R",
         layer="cityiran",driver = "ESRI Shapefile",
         overwrite_layer = T)

setwd ("F:/without external/R/Spatial Data")
getwd ()

#plot shp file by tmap package
library(tmap)
qtm(ostan,fill = "red",style = "natural")
tmap_mode("plot")
qtm(city,dots.col="red", style = "classic")
?qtm
qtm(city,dots.col="red", style = "gray")
qtm(roads,lines.col = "sienna2",lines.lwd = 0.4,style = "gray")

qtm(ostan,fill = "Area",text="NAME",text.size = "AREA",format = "World_wide",
    style = "gray",text.root=5,fill.title="Area (Km sq)")

qtm(ostan_sf,fill = "red",style = "gray")
qtm(city_sf,dots.col="red", style = "gray")
qtm(roads_sf,lines.col = "sienna2",lines.lwd = 0.4,style = "gray")

#cobine shp file
qtm(ostan_sf,fill = "lightblue",style = "gray",
    title = "Iran city & roads\n with Provinces", format = "World_wide")+
  qtm(city,dots.col = "red")+
  qtm(roads,lines.col = "sienna2",lines.lwd = 0.4)

qtm(ostan,fill = "lightblue",style = "gray",
    title = "Iran city & roads\n with Provinces", format = "World_wide")+
  qtm(city_sf,dots.col = "yellow1",symbols.size = 0.4)+
  qtm(roads_sf,lines.col = "sienna2",lines.lwd = 0.2)

qtm(city_sf,dots.col = "yellow1",symbols.size = 0.4,style = "gray",
    format = "World_wide", title = "Iran City")

#use tm_shape for plotting
tm_shape(ostan_sf)+tm_fill ("tomato")

#add the county borders
tm_shape(ostan_sf)+tm_fill ("tomato")+
  tm_borders(lty = "dashed",col="gold")

#add background 
tm_shape(ostan_sf)+tm_fill ("tomato")+
  tm_borders(lty = "dashed",col="gold")+
  tm_style("natural",bg.color="grey90")

#add outline by merge shp file
g<-st_union(ostan_sf)

g_sp<-gUnaryUnion(ostan,id=NULL)

#make a map with outline
tm_shape(ostan_sf)+tm_fill ("tomato")+
  tm_borders(lty = "dashed",col="gold")+
  tm_style("natural",bg.color="grey90")+
  tm_shape(g)+ tm_borders(lwd=2)

tm_shape(ostan_sf)+tm_fill ("tomato")+
  tm_borders(lty = "dashed",col="gold")+
  tm_style("natural",bg.color="grey90")+
  tm_shape(g_sp)+ tm_borders(lwd=2)+
  tm_layout(title = "استان های ایران",title.size = 2,title.position = c(0.75,"top"))
#if you want to make 2 map in plot view
t1<-tm_shape(ostan_sf)+tm_fill ("coral")+tm_borders()+
  tm_layout(bg.color = "grey85")

t2<-tm_shape(g2)+tm_fill("orange")+ tm_borders()+
  tm_layout(asp=0.86,bg.color = "grey95")

library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
#plot t1 ,t2
print(t1,vp=viewport(layout.pos.col = 1,height = 5))
print(t2,vp=viewport(layout.pos.col = 2,height = 5))

#make map without any color and based on Name
tm_shape (ostan_sf)+ tm_fill("white")+tm_borders()+
  tm_text("NAME",size = 0.9)+
  tm_layout(frame = FALSE)

#subsetting sp data based on logical index
View(dfostan)
index <-c(11,15,16)
ostan_sf_sub <- ostan_sf [index,]
ostan_sub <- ostan [index,]
plot(ostan_sf_sub)
plot(ostan_sub)

#make map of subset
tm_shape(ostan_sf_sub)+tm_fill("gold1")+ tm_borders("grey")+
  tm_text("NAME",size=1)+ tm_shape(g_sp)+ tm_borders(lwd = 2)+
  tm_layout(frame = F,title = "North of Iran",
            title.size = 1.5, title.position = c(0,"bottom"))

#make map of iran and then add north provinces
tm_shape(ostan_sf)+tm_fill ("white")+ tm_borders("grey",lwd=0.5)+
  tm_shape(g_sp)+tm_borders(lwd = 2)+
  tm_shape(ostan_sf_sub)+tm_fill("lightblue")+tm_borders()+
  tm_layout(frame = T,title = "Iran with a subset of north provinces",
            title.size = 1,title.position = c(0.02,"bottom"))
#add name
tm_shape(ostan_sf)+tm_fill ("white")+ tm_borders("grey",lwd=0.5)+
  tm_shape(g_sp)+tm_borders(lwd = 2)+
  tm_shape(ostan_sf_sub)+tm_fill("lightblue")+tm_borders()+
  tm_text("NAME",size=1)+
  tm_layout(frame = T,title = "Iran with a subset of north provinces",
            title.size = 1,title.position = c(0.02,"bottom"))

#Add scale bar and compass
tm_shape(ostan)+ tm_borders()+
  tm_shape (roads)+ tm_lines(col="red")+
  tm_scale_bar(width = 0.22,position = c(0.2,0.07))+
  tm_compass(position = c(0.1,0.08))+
  tm_layout(frame = F,title = "Iran provinces and roads",title.size = 1.5,
            title.position = c(0.55,"top"),legend.outside = T)

(tmap_ostan <-tm_shape(ostan)+ tm_borders()+
  tm_shape (roads)+ tm_lines(col="red")+
  tm_scale_bar(width = 0.22,position = c(0.2,0.07))+
  tm_compass(position = c(0.1,0.08))+
  tm_layout(frame = F,title = "Iran provinces and roads",title.size = 1.5,
            title.position = c(0.55,"top"),legend.outside = T))
  
#save map
#make point sf from center of polygon
pts_sf<-st_centroid(ostan_sf)
plot(pts_sf)
class(pts_sf)
plot(pts_sf[,5])

getwd()
#make a png file 
png(filename = "figure2.png",w=5,h=7,units = "in",res = 150)
#make map
tm_shape(ostan_sf)+tm_fill("olivedrab4")+tm_borders("grey",lwd=1)+
  tm_shape(pts_sf)+
  tm_bubbles("Area",title.size="Area (km sq)",col = "gold",
             legend.size.is.portrait=T)+
  tm_format_NLD()
#close the png file
dev.off()

?tmap_save

tmap_save(tmap_ostan,filename = "ostan_tmap.tiff")
tmap_save(tmap_ostan,filename = "ostan_tmap.html")


#Mapping polygon and attributes
tmap_mode("plot")
tm_shape(ostan_sf)+ tm_polygons("Area")
#specify breaks parameters
tm_shape(ostan_sf)+ tm_polygons("Area",breaks=seq(5000,200000,by=20000))

tm_shape(ostan_sf)+ tm_polygons("Area",breaks=c(5000,50000,100000,
                                                150000,200000),title="Area (Km sq)")+
  tm_layout (legend.title.size = 1.2,
             legend.text.size = 0.7,
             legend.position = c("left","bottom"))

#plot 3 shp file together with tmap
tm_shape(ostan_sf)+ tm_polygons("#A6D854")+
  tm_shape(city_sf)+ tm_dots(col = "#D53E4F",size = 0.1,shape = 1,
                             alpha=1)+
  tm_shape(roads_sf)+tm_lines(col = "#B2182B",lwd=0.1)
colors()
library(RColorBrewer)
display.brewer.all()
brewer.pal(5,"Set2")
brewer.pal(8,"RdGy")
brewer.pal(8,"Spectral")

#Clipping shp file
index2 <- ostan$Eng_name=="Golestan" |ostan$Eng_name=="Mazandaran"|
  ostan$Eng_name=="Gilan"
northiran<- ostan[index2,]
#or
northiran_sf <- ostan_sf[index2,]

class(northiran)
class(northiran_sf)
plot(northiran)
plot(northiran_sf[,5])

#map clip shp file
tm_shape(northiran_sf)+tm_borders(col="black")+
  tm_layout(frame = F)+
  tm_shape(city_sf)+tm_dots(col="#A6D854",size = 0.7,shape = 20,alpha=0.5)+
  tm_shape(roads_sf)+tm_lines(col="#3288BD")

#clip north city point
northcity_sf<-city_sf[northiran_sf,]
#clip north roads line
northroads_sf <- roads_sf[northiran_sf,]

tm_shape(northcity_sf)+tm_dots(col = "#A6D854",size = 0.6,shape = 20,alpha=0.5)+
  tm_shape(northroads_sf)+tm_lines(col="#3288BD",lwd=0.07)+
  tm_shape(northiran_sf)+ tm_borders()

#other way 
northc_sf2<-st_intersection(northiran_sf,city_sf)
northr_sf2<-st_intersection(northiran_sf,roads_sf)

tm_shape(northc_sf2)+tm_dots(col = "#A6D854",size = 0.6,shape = 20,alpha=0.5)+
  tm_shape(northr_sf2)+tm_lines(col="#3288BD",lwd=0.07)+
  tm_shape(northiran_sf)+ tm_borders()

#clip with sp class
north.city<-gIntersection(northiran,city,byid = T)
north.road<-gIntersection(northiran,roads,byid = T)
#make clip map
tm_shape(north.city)+tm_dots(col = "red",size = 0.6,shape = 20,alpha=0.5)+
  tm_shape(north.road)+tm_lines(col="#3288BD",lwd=0.07)+
  tm_shape(northiran_sf)+ tm_borders()+
  tm_layout(frame = F)

#plot clipping polygon and attribute
tm_shape(northiran)+ tm_polygons("primeter",title="Perimeter (km)",
                                 pallette="-GnBu")+
  tm_shape(north.city)+tm_dots(col = "red",size = 0.6,shape = 20,alpha=0.5)+
  tm_shape(north.road)+tm_lines(col="#3288BD",lwd=0.07)+
  tm_scale_bar(width = 0.22,position = c("left",0.07))+
  tm_compass(type = "4star",position = c(0,0.1))+
  tm_layout(frame = F,title = "North Of Iran",legend.position = c(0.25,0.5),
            title.size = 2,title.position = c("right",0.95))+
  tm_grid(labels.inside.frame = F,
          n.x = 5,n.y=4)
  
#merge 2 polygon
View(dfostan)
index_G <- ostan$Eng_name=="Golestan"
index_M<- ostan$Eng_name=="Mazandaran"
golestan<- ostan[index_G,]
mazandaran <- ostan[index_M,]
plot(golestan)
plot(mazandaran)

#first method based on sp
mrg.sp<- union(golestan,mazandaran)
plot(mrg.sp)
names(mrg.sp)
dfmrg.sp <- as.data.frame(mrg.sp@data)
dfmrg.sp

#another way using sf package
golestan_sf <- ostan_sf[index_G,]
mazandaran_sf <- ostan_sf [index_M,]
plot(golestan_sf[,6])
plot(mazandaran_sf[,6])

mrg_sf<- rbind(golestan_sf,mazandaran_sf)
names(mrg_sf)
plot(mrg_sf[,6])

#based on st_union
mrg2_sf <- st_union(golestan_sf,mazandaran_sf)
names(mrg2_sf)
plot(mrg2_sf[,6])


#another way
mrg3_sp<-rbind.SpatialPolygonsDataFrame(golestan,mazandaran)
names(mrg3_sp)
plot(mrg3_sp)
dfmrg3_sp <-as.data.frame(mrg3_sp@data)
View(dfmrg3_sp)


