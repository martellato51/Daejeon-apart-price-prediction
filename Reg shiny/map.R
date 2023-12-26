library(raster)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)
###
data=read.csv('data.csv', header = T, encoding = 'UTF-8')

### read shape file
shp=rgdal::readOGR("./sig.shp")
shp=shp[66:70,]

### 좌표 확인: UTM-K(GRS-80) 좌표계에서 WGS84 경위도 좌표계로 변환
from.crs <- "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
shp <- spTransform(shp, to.crs) # 좌표계가 올바르게 변함


pal=colorFactor(palette = 'Dark2', domain = data$시군구)

# 거래금액, 진한 곳은 거래 많은 곳
leaflet(data) %>%
    addProviderTiles('CartoDB.Positron') %>% 
    setView(lng=127.384633005948, lat=36.3503849976553, zoom=12) %>% 
    addPolygons(data = shp,weight=3,fillOpacity = 0.1) %>% 
    addCircles(lng = ~경도, lat = ~위도, radius = ~거래금액/100, color = ~pal(시군구), fillOpacity = 0.1,
               weight = 1.5, popup = ~htmlEscape(paste(아파트, 전용면적))) %>% 
    addLegend("topright", pal=pal,values=~unique(시군구), opacity = 0.7, title = "시군구") 


# 전용면적, 진한 곳은 거래 많은 곳
leaflet(data) %>%
    addProviderTiles('CartoDB.Positron') %>% 
    setView(lng=127.384633005948, lat=36.3503849976553, zoom=12) %>% 
    addPolygons(data = shp,weight=3,fillOpacity = 0.1) %>% 
    addCircles(lng = ~경도, lat = ~위도, radius = ~전용면적*5, color = ~pal(시군구), fillOpacity = 0.1,
               weight = 1.5, popup = ~htmlEscape(전용면적)) %>% 
    addLegend("topright", pal=pal,values=~unique(시군구), opacity = 0.7, title = "시군구") 


# 최고층
leaflet(data) %>%
    addProviderTiles('CartoDB.Positron') %>% 
    setView(lng=127.384633005948, lat=36.3503849976553, zoom=12) %>% 
    addPolygons(data = shp,weight=3,fillOpacity = 0.1) %>% 
    addCircles(lng = ~경도, lat = ~위도, radius = ~최고층*20, color = ~pal(시군구), fillOpacity = 0.1,
               weight = 1.5,
               popup = ~htmlEscape(최고층)) %>% 
    addLegend("topright", pal=pal,values=~unique(시군구), opacity = 0.7, title = "시군구") 


# 학원수
leaflet(data) %>%
    addProviderTiles('CartoDB.Positron') %>% 
    setView(lng=127.384633005948, lat=36.3503849976553, zoom=12) %>% 
    addPolygons(data = shp,weight=3,fillOpacity = 0.1) %>% 
    addCircles(lng = ~경도, lat = ~위도, radius = ~학원*1.5, color = ~pal(시군구), fillOpacity = 0.1,
               weight = 1.5,
               popup = ~htmlEscape(최고층)) %>% 
    addLegend("topright", pal=pal,values=~unique(시군구), opacity = 0.7, title = "시군구")



