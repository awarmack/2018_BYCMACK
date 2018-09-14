#leaflet based Shiny App

library(shiny)
library(leaflet)
library(tidyverse)
library(scales)
#library(viridis)

load("../../mod_data/positions_wcorr.Rdata")
exp <- read.csv("../../mod_data/expdition_5minavg.csv", stringsAsFactors = FALSE)


posn <- posn %>% filter(class=="Class O Class")

boats <- unique(posn$boat)


ui <- fluidPage(
  fluidRow(
    column(6, 
           "time", 
           sliderInput("endtime", "Visible Time",
                       min=as.POSIXct("2018-07-14 16:00:00 EDT"), 
                       max=as.POSIXct("2018-07-17 24:00:00 EDT"), 
                       value=c(as.POSIXct("2018-07-14 16:00:00 EDT"), as.POSIXct("2018-07-17 24:00:00 EDT") ), 
                       timeFormat = "%a %H:%M", animate = TRUE, width = "100%")), 
    column(6, 
           "other options", 
           selectInput(inputId = "vsboat", label="Versus (in Red)", choices=boats),
           checkboxInput("corrtime", "Use Corrected Time", value=FALSE))
  ), 
  
  fluidRow(
    column(3,
           "Yellowbrick",
           plotOutput("dmgPlot", width = 300, height = 200), 
           plotOutput("speed", width=300, height=200),
           plotOutput("speedhist", width=300, height = 200)
    ),
    column(5,
           "Position",
           leafletOutput("mymap", height=800)
    ), 
    column(4, 
           "Expidition Data",
           plotOutput("polar", width=400,height=400), 
           plotOutput("perf", width=400, height=400))
  )
)


server <- function(input, output, session) {
  
  
  
  #posn$boat <- as.factor(posn$boat)
  #classlevels <- unique(df$boat)
  #factpal <- colorFactor(palette = "viridis", levels=classlevels)
  
  trimData <- reactive({
    
    if(input$corrtime){
      posn$time <- posn$corr_time
    }
    
    #filter position data by time
    posn[posn$time > input$endtime[1] & posn$time < input$endtime[2], ]
    
    
    
  })
  
  
  perfData <- reactive({
    
    exp[exp$time > input$endtime[1] & exp$time < input$endtime[2], ]
    
  })
  
  
  output$mymap <- renderLeaflet({
    
    m <- leaflet() %>%  addTiles()
    #m <- m %>% addPolylines(data=df, lng=~lon, lat=~lat)
    #m <- m %>% addPolylines(data=tuna, lng=~lon, lat=~lat, color="red")
    
    posn <- trimData()
    
    perf <- perfData()
    
    posn$timelabel <- format(posn$time, "%a %R")
    
    zubepos <- posn[posn$boat == "Zubenelgenubi", ]
    m <- m %>% addPolylines(data=zubepos, lng=~lon, lat=~lat, color="blue", weight=2, opacity = 100, label=~boat)
    
    vs <- posn[posn$boat == input$vsboat, ]
    m <- m %>% addPolylines(data=vs, lng=~lon, lat=~lat, 
                            color="red", weight=2, opacity=100, label=~boat)
    
    
    m <- m %>% addCircleMarkers(data=perf, lng=~lon, lat=~lat, color=~pol_perc, radius = 3)
    

  })
  
  output$dmgPlot <- renderPlot({
    
    posn <- trimData()  
    
    df <- posn[posn$boat == "Zubenelgenubi" | posn$boat == input$vsboat,  ]
    
    ggplot(posn) + 
      geom_path(data=df, aes(x=time, y=dmg, color=boat))+
      scale_y_continuous(name="Distance Made Good to Finish [NM]")+
      scale_x_datetime(labels=date_format("%a %R"))+
      theme(legend.position="top")+
      scale_color_manual(values=c("red", "blue"))
  })
  
  #Speed Over Time
  output$speed <- renderPlot({
    posn <- trimData()
    
    posn <- posn[posn$boat == "Zubenelgenubi" | posn$boat == input$vsboat,  ]
    
    ggplot(posn)+
      geom_path(aes(x=time, y=SOG, color=boat))+
      scale_y_continuous(name="SOG: Speed Over Ground [kts]")+
      scale_x_datetime(labels=date_format("%a %R"))+
      theme(legend.position="none")+
      scale_color_manual(values=c("red", "blue"))
  })
  
  #Histogram of Speed Over Ground
  output$speedhist <- renderPlot({
    posn <- trimData()
    
    posn <- posn[posn$boat == "Zubenelgenubi" | posn$boat == input$vsboat,  ]
    
    ggplot(posn)+
      geom_density(aes(x=SOG, fill=boat), alpha=0.2)+
      theme(legend.position="none")+
      scale_fill_manual(values=c("red", "blue"))
    
  })
  
  output$polar <- renderPlot({
    zube <- perfData()

    zube <- zube[zube$twa < 180, ]

    zube$brks <- cut(zube$pol_perc, breaks=c(0, 50, 75, 100, 125, 150))

    #zube$brks<- cut_number(zube$pol_perc, n = 5)
    #zube$brks<- str_replace(zube$brks, pattern = "\\,", replacement = "~")


    ggplot(zube)+geom_point(aes(x=twa, y=sog, color=brks))+
      #scale_color_brewer(type = "div")+
      scale_color_viridis_d()+
      scale_x_continuous(limits=c(-180,180))+
      coord_polar(start=pi)
  })

  output$perf <- renderPlot({
    zube <- perfData()

    zube <- zube[zube$twa < 180, ]

    zube$brks <- cut(zube$pol_perc, breaks=c(0, 50, 75, 100, 125, 150))

    ggplot(zube)+geom_point(aes(x=time, y=pol_perc, color=brks))+
      #scale_color_brewer(type = "div")+
      scale_color_viridis_d()+
      scale_y_continuous(limits=c(0,150))

  })
  
  
}

shinyApp(ui, server)