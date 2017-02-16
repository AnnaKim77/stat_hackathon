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
library(lmtest)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(magrittr)
})

rm(list=ls(all=TRUE))

setwd("C:/Users/user/Desktop/hackathon/dataset")
metal <- read.csv("metal.csv",header = T)
grid <- read.csv("seoul_grid.csv",header = T)
metal_kri <- read.csv("metal_kri.csv",header = T)

metal$Month <- as.factor(metal$Month)
metal_kri$Month <- as.factor(metal_kri$Month)
metal$Year <- as.factor(metal$Year)
metal_kri$Year <- as.factor(metal_kri$Year)

metal <- merge(metal, grid, by="Loca", all.x = T)
metal_kri <- merge(metal_kri, grid, by="Loca", all.x = T)

summary(metal)

corrplot(cor(metal[,c(-1,-8,-9,-10)]),method = "number")
corrplot(cor(metal[,c(-1,-8,-9,-10)]),method = "circle")

for(i in c(3,4,5,6,7,8)){
  metal[,i] <- scale(metal[,i])
}

for(i in c(3,4,5,6,7)){
  metal_kri[,i] <- scale(metal_kri[,i])
}

met_lm <- lm(log(Pb)~.,data = metal[,c(-1,-8,-9,-10)])

summary(met_lm)

par(mfrow = c(2,2))
plot(met_lm)
par(mfrow = c(1,1))

step <- step(met_lm, direction = "backward")

met_lm_red <- lm(log(Pb) ~ NO2+O3+CO+PM25, data = metal)
summary(met_lm_red)
vif(met_lm_red)

par(mfrow =c(2,2))
plot(met_lm_red)
par(mfrow =c(1,1))

ncvTest(met_lm_red)

shapiro.test(residuals(met_lm_red))

metal$resid = residuals(met_lm_red)

#month/year 데이터set 구성

metal_mon1 <- metal[metal$Year == 2015,]
metal_kri_mon1 <- metal_kri[metal_kri$Year == 2015,]

summary(metal_kri)

myMap <- get_map(location="seoul", source="google", maptype="roadmap", crop=FALSE, zoom = 11) 

plot1 = ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                                data = metal_mon1, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points with measurements") + theme(plot.title = element_text(hjust = 0.5))

plot2=ggmap(myMap)+ geom_point(aes(x = lon, y = lat), 
                               data = metal_kri_mon1, alpha = .5, color="darkred", size = 1) + coord_equal() + 
  ggtitle("Points at which to estimate") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, ncol = 2)

#coordinates(metal_mon1) = ~ lat + lon
#bbox(metal_mon1)

#sample variogram values
#dvgm <- variogram(resid ~ 1, data = metal_mon1)  
#dfit <- fit.variogram(dvgm, vgm("Gau"))
#plot(dvgm)
#plot(dvgm, dfit)
#
#kriged <- krige(log(resid) ~ 1, metal_2010, metal_kri_2010, model=dfit)
#dk=as.data.frame(kriged)

metal_kri_mon1$pred <- exp(predict(met_lm_red,metal_kri_mon1))

x1.mm=metal_kri_mon1$lon
x2.mm=metal_kri_mon1$lat

x1r = range(metal_kri$lon)
x2r = range(metal_kri$lat)
y1 = metal_kri_mon1$pred

mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])

int.data=interp(x1.mm,x2.mm,y1,duplicate="strip")
map("world",xlim=x1r,ylim=x2r,lwd=2)
image(int.data,col=heat.colors(100),add=T)
contour(int.data,add=T,col=4,lwd=1,labcex=1)
map("world",xlim=x1r,ylim=x2r,lwd=2,add=T)
