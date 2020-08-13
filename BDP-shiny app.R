library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)


ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "BPD Arrest", 
                                enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
            menuItem("Density", tabName = "page2", icon = icon("area-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Data",tabName="page4",icon=icon("database")))
                              ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)
            ),
            tabItem(tabName = "page2",
                sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                            step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                plotOutput("plot1")
            ),


            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    dataTableOutput("myTable")
                   )
              )
          ),
    rightsidebar = rightSidebar(
        tags$a("Data Source",href="https://data.baltimorecity.gov/Public-Safety/BPD-Arrests/3i3v-ibrt"),target="_blank"
                                ),
    title = "DashboardPage"
)


server <- function(input, output, session) {

    
    #### b) Data Wrangling 
    data=read_csv('data.csv')

    data$ArrestDate <- as.Date(data$ArrestDate, format="%m/%d/%Y")
    
    data$Longitude=round(data$Longitude,digits=5)
    
    data$Latitude=round(data$Latitude,digits=5) 
    
#part d first 5 bullets 
    holidays=read_csv("usholidays.csv") #read_csv is the function in tydiverse, it doesnt read character as factor whereas the read.csv stores the character as factor.
    holidays$Date=as.Date(holidays$Date, format="%m/%d/%Y")
    
    words=unique(holidays$Holiday) #

    Abb=c("NYD","MLKB","WaB", "MeD", "InD", "LaD", "CoD", "VeD", "ThD", "ChD","NYD","MLKB","WaB")
    holidays$Abb=holidays$Holiday
    for (i in 1:length(words)) {
        holidays$Abb=str_replace(holidays$Abb,words[i],Abb[i])
    }
    
   
    
output$plot1 = renderPlot({
        

        
        ##### c) Density Graph
        data<-data %>% filter(as.numeric(format(ArrestDate,'%Y'))==input$year) 
        
        graph<- ggplot(data=data,mapping=aes(x=Age))+aes(color=Sex)+
            annotate(geom="text", color= "grey80",x=45,y=0.025,label=input$year,size=20)+
            geom_density(size=1)+xlim(10,80)+ 
            ylim(0,0.05)+labs(y="Density")+
            theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank())+scale_color_discrete(labels=c("Female","Male"))+
            labs(x = "Age", y = "Density",title = "Age distribution of crimes reported within each gender")+
            guides(color=guide_legend(title = "Gender"))
        
        graph
    })
    
output$plot2 = renderPlotly({
    
    
    #### d) Plotly Grapah
    data_hol=data %>%group_by(Date=ArrestDate) %>% summarise(N=n())%>%merge(holidays,all.x=TRUE)
    
    #cant use mapping=(aes(),size()) bc mapping=aes(x=  ,y=   ,size=  )
    f=ggplot(data=data_hol,mapping =aes(x=Date,y=N))+geom_line()+geom_smooth()
    
    
    if (input$holiday==TRUE){ f=f+
        geom_point(data= subset(data_hol, !is.na(Holiday)), color="purple")+
        geom_text(data=subset(data_hol,!is.na(Holiday )), aes(x=Date, y=N, label=Abb)) }
    
        ggplotly(f)
        
    })
    

    
output$myMap = renderLeaflet({
        #### e) Map
        
        loc_data= data %>%
            group_by(lng=round(Longitude,3),lat=round(Latitude,3)) %>%
            summarise(N=n())
        
        
        loc_data$latL =loc_data$lat-0.0005
        loc_data$latH =loc_data$lat+0.0005
        loc_data$lngL =loc_data$lng-0.0005
        loc_data$lngH=loc_data$lng+0.0005
        
        #create rectangular
        
        m=loc_data %>%
            leaflet() %>% addTiles() %>%
            setView( -76.6352, 39.3103, zoom=12) %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
            addLayersControl( baseGroups = c("Toner", "OSM"),
                              options = layersControlOptions(collapsed = FALSE))%>%
            addRectangles(
                lng1=~lngL, lat1=~latL,
                lng2=~lngH, lat2=~latH,
                fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N
            )
        
        m    
        })

output$myTable= DT::renderDataTable({
    
    
    return(datatable(data, rownames= FALSE))
})
    
    
}

shinyApp(ui = ui, server = server)
