####library
library(MASS)
library(plyr)
library(pscl)
library(splines)
library(ggmap)
library(sp)
library(gstat)
library(gridExtra)
library(akima)
library(maps)
library(car)
library(corrplot)
library(ggplot2)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(magrittr)
})

#data eraser
rm(list=ls(all=TRUE))

#working directory
setwd("C:/Users/user/Desktop/hackathon/dataset")
metal <- read.csv("metal_or.csv",header = T)
grid <- read.csv("seoul_grid.csv",header = T)
metal_kri <- read.csv("metal_kri.csv",header = T)

metal$Month <- as.factor(metal$Month)
metal_kri$Month <- as.factor(metal_kri$Month)
metal$Year <- as.factor(metal$Year)
metal_kri$Year <- as.factor(metal_kri$Year)

#좌표값 입력
metal <- merge(metal, grid, by="Loca", all.x = T)
metal_kri <- merge(metal_kri, grid, by="Loca", all.x = T)

summary(metal)

corrplot(cor(metal[,c(-1,-8,-9,-10)]),method = "number")
corrplot(cor(metal[,c(-1,-8,-9,-10)]),method = "circle")

met_lm <- lm(log(Pb)~.,data = metal[,c(-1,-8,-9,-10)])

summary(met_lm)

par(mfrow = c(2,2))
plot(met_lm)
par(mfrow = c(1,1))

step <- step(met_lm, direction = "backward")

met_lm_red <- lm(log(Pb) ~ NO2 + O3 + CO + PM25, data = metal)

summary(met_lm_red)
vif(met_lm_red)

par(mfrow =c(2,2))
plot(met_lm_red)
par(mfrow =c(1,1))

ncvTest(met_lm_red)

shapiro.test(residuals(met_lm_red))

#예측지역 시각화
myMap <- get_map(location="seoul", source="google", maptype="roadmap", crop=FALSE, zoom = 11) 
seoul_map <- qmap('Seoul', zoom = 11, source = 'stamen', maptype = 'toner')
seoul_map

plot1 = ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                                data = metal, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points with measurements") + theme(plot.title = element_text(hjust = 0.5))

plot2=ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                               data = metal_kri, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points at which to estimate") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, ncol = 2)

#회귀직선이용 예측값사용
metal_kri$pred <- exp(predict(met_lm_red,metal_kri))

Pb.colors <- c("#FFFFF4FF","#FFFFDFFF","#FFFFCAFF","#FFFFB5FF","#FFFF9FFF","#FFFF8AFF","#FFFF75FF","#FFFF60FF","#FFFF4AFF","#FFFF35FF","#FFFF35FF","#FFFF20FF","#FFFF0BFF","#FFFF00FF","#FFF800FF","#FFF100FF",
               "#FFEA00FF","#FFE300FF","#FFDD00FF","#FFD600FF","#FFCF00FF","#FFC800FF","#FFC100FF","#FFBA00FF","#FFB300FF","#FFAC00FF","#FFA500FF","#FF9F00FF","#FF9800FF","#FF9100FF","#FF8A00FF","#FF8300FF",
               "#FF7C00FF","#FF7500FF","#FF6E00FF","#FF6700FF","#FF6000FF","#FF5A00FF","#FF5300FF","#FF4C00FF","#FF4500FF","#FF3E00FF","#FF3700FF","#FF3000FF","#FF2900FF")

#예측시각화 (2015년)
x1.15 <- metal_kri[metal_kri$Year == 2015,10]
x2.15 <- metal_kri[metal_kri$Year == 2015,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2015 <- metal_kri[metal_kri$Year == 2015,12]

int.data = interp(x1.15, x2.15, y2015, duplicate="strip")
map('world',xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

states <- data.frame(x1.15,x2.15,y2015)
head(states)

ggplot(data = states) + 
  geom_polygon(aes(x = x1.15, y = x2.15, fill = y2015), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

#예측시각화 (2014년)
x1.14 <- metal_kri[metal_kri$Year == 2014,10]
x2.14 <- metal_kri[metal_kri$Year == 2014,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2014 <- metal_kri[metal_kri$Year == 2014,12]

int.data=interp(x1.14, x2.14, y2014, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2013년)
x1.13 <- metal_kri[metal_kri$Year == 2013,10]
x2.13 <- metal_kri[metal_kri$Year == 2013,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2013 <- metal_kri[metal_kri$Year == 2013,12]

int.data=interp(x1.13, x2.13, y2013, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2012년)
x1.12 <- metal_kri[metal_kri$Year == 2012,10]
x2.12 <- metal_kri[metal_kri$Year == 2012,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2012 <- metal_kri[metal_kri$Year == 2012,12]

int.data=interp(x1.12, x2.12, y2012, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2011년)
x1.11 <- metal_kri[metal_kri$Year == 2011,10]
x2.11 <- metal_kri[metal_kri$Year == 2011,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2011 <- metal_kri[metal_kri$Year == 2011,12]

int.data=interp(x1.11, x2.11, y2011, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2010년)
x1.10 <- metal_kri[metal_kri$Year == 2010,10]
x2.10 <- metal_kri[metal_kri$Year == 2010,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2010 <- metal_kri[metal_kri$Year == 2010,12]

int.data=interp(x1.10, x2.10, y2010, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=Pb.colors,add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

##추가
require(Kormaps)
require(tmap)

install.packages("maptools")
library(maptools)

maptools::readShapePoly("KOR_adm2.shp")

shp2 <- readShapePoly("C:\\Users\\user\\Downloads\\KOR_adm_shp\\KOR_adm2.shp")
str(shp2@data)

ggplot2::fortify()
shp2_ffd2 <- fortify(shp2, region = "NAME_2")
shp2_subset <- shp2[shp2$NAME_1 == "Seoul",]
seoul_ffd <- fortify(shp2_subset, region="NAME_2")
ggplot(seoul_ffd, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=id))

seoul_ffd

seoul_value <- data.frame(
  seoul_ffd %>% 
    select(id) %>%
    distinct,
  value = round(runif(16,10,100)))

shp2_value
