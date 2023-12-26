install.packages('leafletProxy')
library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Choose Variable", choices = c("거래금액", "전용면적","최고층","학원")),
            actionButton("confirmBtn", "Confirm")
        ),
        mainPanel(
            leafletOutput("map",height = 1000)
        )
    )
)

server <- function(input, output) {
        data=read.csv('data.csv', header = T, encoding = 'UTF-8')
        ### read shape file
        shp=rgdal::readOGR("./sig.shp")
        shp=shp[66:70,]
        
        ### 좌표 확인: UTM-K(GRS-80) 좌표계에서 WGS84 경위도 좌표계로 변환
        from.crs <- "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
        to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        shp <- spTransform(shp, to.crs) # 좌표계가 올바르게 변함
        observeEvent(input$confirmBtn, {
        pal <- colorFactor(palette = 'Dark2', domain = data$시군구)

        option=data.frame('data$거래금액/100',
                 'data$전용면적*5',
                 'data$최고층*20',
                 'data$학원*2')
        colnames(option)=c("거래금액", "전용면적","최고층","학원")
        
        
        
        output$map <- renderLeaflet({
            leaflet(data) %>%
                addProviderTiles('CartoDB.Positron') %>% 
                setView(lng = 127.384633005948, lat = 36.3503849976553, zoom = 12) %>% 
                addPolygons(data = shp, weight = 3, fillOpacity = 0.1) %>% 
                addCircles(
                    lng = ~경도, lat = ~위도,
                    radius = eval(parse(text=option[1,input$variable])),
                    color = ~pal(시군구), fillOpacity = 0.1,
                    weight = 1.5, popup = ~htmlEscape(paste(아파트, 전용면적))
                ) %>% 
                addLegend("topright", pal = pal, values = ~unique(시군구), opacity = 0.7, title = "시군구") 
        })
    })
}

shinyApp(ui, server)

