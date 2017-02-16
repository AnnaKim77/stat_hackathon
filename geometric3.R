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

plot1 = ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                                data = metal, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points with measurements") + theme(plot.title = element_text(hjust = 0.5))

plot2=ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                               data = metal_kri, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points at which to estimate") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, ncol = 2)

#회귀직선이용 예측값사용
metal_kri$pred <- exp(predict(met_lm_red,metal_kri))


#예측시각화 (2015년)
x1.15 <- metal_kri[metal_kri$Year == 2015,10]
x2.15 <- metal_kri[metal_kri$Year == 2015,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2015 <- metal_kri[metal_kri$Year == 2015,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.15, x2.15, y2015, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2014년)
x1.14 <- metal_kri[metal_kri$Year == 2014,10]
x2.14 <- metal_kri[metal_kri$Year == 2014,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2014 <- metal_kri[metal_kri$Year == 2014,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.14, x2.14, y2014, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2013년)
x1.13 <- metal_kri[metal_kri$Year == 2013,10]
x2.13 <- metal_kri[metal_kri$Year == 2013,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2013 <- metal_kri[metal_kri$Year == 2013,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.13, x2.13, y2013, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2012년)
x1.12 <- metal_kri[metal_kri$Year == 2012,10]
x2.12 <- metal_kri[metal_kri$Year == 2012,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2012 <- metal_kri[metal_kri$Year == 2012,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.12, x2.12, y2012, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2011년)
x1.11 <- metal_kri[metal_kri$Year == 2011,10]
x2.11 <- metal_kri[metal_kri$Year == 2011,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2011 <- metal_kri[metal_kri$Year == 2011,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.11, x2.11, y2011, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)

#예측시각화 (2010년)
x1.10 <- metal_kri[metal_kri$Year == 2010,10]
x2.10 <- metal_kri[metal_kri$Year == 2010,11]
x1r <- range(metal_kri$lon)
x2r <- range(metal_kri$lat)
y2010 <- metal_kri[metal_kri$Year == 2010,12]

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.10, x2.10, y2010, duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)