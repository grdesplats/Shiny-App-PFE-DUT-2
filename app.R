library(leaflet)
library(leaflet.extras)
library(shiny)
library(tidyverse)
library(rvest)
library(shinythemes)
library(shinyBS)

earthquake = read.csv("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.csv")

earthquake$time = earthquake$time %>% as.Date()

earthquake = earthquake %>% mutate(Age = case_when(
  
  time == Sys.Date() ~ "Day",
  time + 7 >= Sys.Date() ~ "Week",
  TRUE ~ "Month"
  
))

quakes.df = earthquake %>%
  mutate(
      mag.level = cut(earthquake$mag, 
      breaks = c(2.5, 4.5, 5.5, 10), right=FALSE,
      labels = c("Low [2.5-4.5]", "Moderate [4.5-5.5]", "Strong[5.5-10)"))
  ) %>%
  split(.$Age)

pal <- colorFactor(c("yellow","orange","red"), domain = earthquake$mag.level)

labels = c("Low [2.5-4.5]", "Moderate [4.5-5.5]", "Strong[5.5-10)")

map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18,preferCanvas = F )) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto Positron") %>%
  setView(lng = 4, lat = 0, zoom = 2) %>% 
  addScaleBar() %>% 
  addLayersControl(
    position = c("topleft"),
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"><b>Display Earthquakes since last...</b></label>');
        }
    ") %>% 
  addLegend(
    title = "Magnitude Range",
    pal = pal,
    values = labels,
    opacity = 1,
    position = "bottomleft") %>% 
  hideGroup(names(quakes.df)) %>% 
  setMaxBounds(lng1 = -210, lng2 = 210, lat1 = -1200, lat2 = 1200)


names(quakes.df) %>%
  purrr::walk( function(df) {
    map <<- map %>%
      addCircleMarkers(data=quakes.df[[df]], lng=~longitude, lat=~latitude,
                       radius = 5,
                       color = ~pal(mag.level),
                       label=~as.character(paste0("magnitude : ",mag,' date : ',time)),
                       group = df)
  })



#Adding Tectonic plates

dbTectonic <- read_csv("database/tectonic.csv") %>% unique()
dbTectonic$lat = as.numeric(dbTectonic$lat)
dbTectonic$lon = as.numeric(dbTectonic$lon)
dbTectonic$plate = str_to_upper(dbTectonic$plate)


plates = unique(dbTectonic$plate)

for (plate1 in plates){
  
  data = dbTectonic[(dbTectonic$plate == plate1),] %>% unique() 
  BorneInf = 0
  
  for (i in 1:nrow(data)){
    
    if(i + 1 <= nrow(data)){
      
      if(abs(data$lon[i]-data$lon[i + 1])>300){
        
        table1 = data %>% slice((BorneInf + 1):i)
        BorneInf = i
        map = map %>% 
          addPolylines(data = table1, lat = ~lat,lng =~lon,weight = 2,color = "grey", group = "tectonic")
        
      }
      
    }
    
  }
  
  table1 = data %>%  slice((BorneInf + 1):nrow(data))
  map = map %>% addPolylines(data = table1, lat = ~lat,lng =~lon,weight = 2,color = "grey", group = "tectonic")
}

#Volcanos

dbVolcano = read_delim("database/volcano.csv", delim = ";")
dbVolcano$latitude = as.numeric(dbVolcano$latitude)
dbVolcano$elevation = as.numeric(dbVolcano$elevation)
dbEruptions = read_csv("database/eruptions.csv")


performance=function(age,perf) {
  db_kp=data.frame(age,perf)
  db_kp=na.omit(db_kp)
  perf = data.frame(db_kp %>% group_by(age) %>% summarise("Max VEI"=max(perf)))
  return(perf)
}
volcano_max_vei = performance(dbEruptions$volcano_name,dbEruptions$vei)

dbVolcano = left_join(dbVolcano,volcano_max_vei, by = c("volcano_name" = "age")) 

  
source("VolcanoCard.R")

ui = navbarPage("Discover Volcanoes", id="nav",
           
                theme = shinytheme("sandstone"),
                
           tabPanel("Interactive map",
                    
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("www/styles.css")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        tags$div(id="title",
                                 ),
                        absolutePanel(
                          top = 230, left =15, style = "z-index:500;",
                          checkboxInput("tect",tags$b("Display tectonics plates boundaries"),FALSE),
                          checkboxInput("volc",tags$b("Display Volcanoes"),TRUE)
                        ),
                        
                        tags$div(id="cite",
                                 'Data from ', tags$em('Earthquake Hazards Program, Real-time feeds'), ' by', tags$a(href = "https://www.usgs.gov/natural-hazards/earthquake-hazards/earthquakes",target = "_blank",'USGC')
                        )
                    )
           ),
           
          tabPanel("Info",icon = icon("info-circle"),
                   fluidRow(
                     column(2),
                     column(8,
                        HTML('<h2 class="entry-title">Informations </h2>'),
                        tags$div(id = "entry-title2"),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                      'On this application, you can discover the different volcanoes present on Earth. Click on the volcano icons to get more information from Wikipedia (using scraping). You can remove the icons by unchecking "show volcanoes".
                      Moreover, you can access the  earthquakes of the last month, by checking the box "Display Earthquakes since last...". The database is automatically updated with data from the U.S. Geological Survey (USGC) ',
                      
                        tags$br(),
                        tags$br(),
                      "View code on github",
                      tags$a(class="linkedin", href="https://github.com/grdesplats/Volcanoes-Shiny-App", target="_blank",
                             tags$img(class="", src="github.png", width = "2%"))
                     ),
                   ),
                   )
           

)

server <- function(input, output) {
  
    
    output$map = renderLeaflet({
      map %>% 
        hideGroup("tectonic")
    })
    
    
    # tectonic plates
    observe({
      if(input$tect){
        leafletProxy("map") %>% showGroup("tectonic")
      } else if(!input$tect){
        leafletProxy("map") %>% hideGroup("tectonic")
      }
    })
    
    observe({
      if(input$volc){
        leafletProxy("map") %>% showGroup("data")
      } else if(!input$volc){
        leafletProxy("map") %>% hideGroup("data")
      }
    })
    
    # Volcanos Icon
    observe({
      leafletProxy("map", data = dbVolcano) %>%
        addMarkers(~longitude,
                   ~latitude,
                   icon = makeIcon(
                     iconUrl = "www/volcano-icon-12.jpg",
                     iconWidth = 40,
                     iconHeight = 40
                   ),
                   group = "data",
                   clusterOptions = markerClusterOptions()
                   )
    })
    
    
    #Volcanos Card
    observeEvent(input$map_marker_click, { 
      pin <- input$map_marker_click
      selectedPoint <- reactive(dbVolcano[dbVolcano$latitude == pin$lat & dbVolcano$longitude == pin$lng,])
      leafletProxy("map", data = selectedPoint()) %>% clearPopups() %>% 
        addPopups(~longitude,
                  ~latitude,
                  popup = ~volcano_card(selectedPoint()$volcano_name,selectedPoint()$Max.VEI,selectedPoint()$primary_volcano_type,selectedPoint()$elevation,selectedPoint()$last_eruption_year),
                  options = popupOptions(closeButton = FALSE)
        )
    })
    
    observeEvent(input$map_circle_marker_click,{
      
        click <- input$map_circle_marker_click
        selected <- earthquake[earthquake$latitude == click$lat & earthquake$longitude == click$lng,]
        # select2 <- dbTectonic[dbTectonic$lat == click$lat & dbTectonic$lon == click$lng,]
        text <- as.character(tagList(
          paste0("Magnitude: ", selected$mag), tags$br(),
          paste0("Year: ", selected$time), tags$br(),
          paste0("Depth: ", selected$depth), tags$br(),
          paste0("Type: ", selected$type),tags$br(),
          # paste0(select2$lat),tags$br(),
          # paste0(select2$lon)
        ))
      leafletProxy("map") %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
      
    })

}

shinyApp(ui = ui, server = server)