

library(shinythemes)
library(ggplot2)
library(shiny)
library(stringr)
library(leaflet)
library(plotly)
library(DT)
ozona_hr <- readRDS("data/hourly_outside_zona_finalMon_Feb_14.rds")
ozona <- readRDS("data/instant_outside_zona_finalMon_Feb_14.rds")
izona_hr <- readRDS("data/hourly_inside_zona_finalMon_Feb_14.rds")
izona <- readRDS("data/instant_inside_zona_finalMon_Feb_14.rds")
oestrella_hr <- readRDS("data/hourly_outside_estrella_finalMon_Feb_14.rds")
oestrella <- readRDS("data/instant_outside_estrella_finalMon_Feb_14.rds")
iestrella_hr <- readRDS("data/hourly_inside_estrella_finalMon_Feb_14.rds")
iestrella <- readRDS("data/instant_inside_estrella_finalMon_Feb_14.rds")
# User interface ----
ui <-
  navbarPage("IQ Air Devices in Torreon, Mexico", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
           ## Home page ----
             tabPanel(icon("home"),
                    fluidRow(
                     column(
                       
                       br(),
                       p("This Shiny Application was built to support
                         the IQ Air Visual Pro Device Data Dashboard. As of 2/4/2022, we
                         have four devices installed in Torreon, Mexico.These devices are logging instantaneous data, and this dashboard shows hourly and instant data findings.",
                         style="text-align:justify;color:black;background-color:light blue;padding:15px;border-radius:10px"),
                       br(),
                       
                       p("Explore the tabs at the top to get started!", 
                       style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                       
                       width=12)),
                    
                    hr(),
                    
                    p(em("Developed: February 2022"),br("Author: Flannery Black-Ingersoll, MPH"),
                      em("for M. Patricia Fabian, ScD, BUSPH"),
                      style="text-align:center; font-family: times")),
           ## Zona page ----
           tabPanel("Industrial Zone",
                    fluidPage(
                      tabsetPanel(
                        tabPanel("Time Series Plots", br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                   selectInput("var1", 
                                                 label = "Hourly Outdoor",
                                                 choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                             "AQI (US AQI)","Temp (degrees F)",
                                                             "CO2 (ppm)","Humidity (%)"), 
                                                 selected = "PM2.5"),
                                     selectInput("var2", 
                                                 label = "Hourly Indoor",
                                                 choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                             "AQI (US AQI)","Temp (degrees F)",
                                                             "CO2 (ppm)","Humidity (%)"), 
                                                 selected = "PM2.5"),
                                     selectInput("var3", 
                                                 label = "15-min Outdoor",
                                                 choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                             "AQI (US AQI)","Temp (degrees F)",
                                                             "CO2 (ppm)","Humidity (%)"), 
                                                 selected = "PM2.5"),
                                     selectInput("var4", 
                                                 label = "15-min Indoor",
                                                 choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                             "AQI (US AQI)","Temp (degrees F)",
                                                             "CO2 (ppm)","Humidity (%)"), 
                                                 selected = "PM2.5"),
                                 
                                     br()),
                                   mainPanel(plotlyOutput("plot1a"),
                                             plotlyOutput("plot1b"),
                                             plotlyOutput("plot1c"), 
                                             plotlyOutput("plot1d"),
                                             plotlyOutput("plot1e"))
                                   )),
                        tabPanel("Map", br(),
                                 
                                 helpText(h4("Zona Industrial, Torreon, Mexico")),
                                                                  br(),
                                 leafletOutput("map1", width="1000", height="1000"),
                                 br()),
                        tabPanel("Hourly Data Table: Outdoors", 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     br(strong("Variables:")),
                                 br("p2 = PM2.5 (ug/m^3)"), 
                                  br("p1 = PM10 (ug/m^3)"), 
                                 br("co = CO2 (ppm)"), 
                                 br("p01 = AQI (US AQI)"), 
                                   br("tp = TEMP(degrees F)"),
                                    br("hm = Humidity (%)")),
                            mainPanel(
                                 DT::dataTableOutput("table1"),
                                 br()))),
                        tabPanel("Hourly Data Table: Indoors", br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     br(strong("Variables:")),
                                     br("p2 = PM2.5 (ug/m^3)"), 
                                     br("p1 = PM10 (ug/m^3)"), 
                                     br("co = CO2 (ppm)"), 
                                     br("p01 = AQI (US AQI)"), 
                                     br("tp = TEMP(degrees F)"),
                                     br("hm = Humidity (%)")),
                                   mainPanel(
                                     DT::dataTableOutput("table2"),
                                     br()))),
                      ))),
           
           ## Estrella page ----
              tabPanel("Estrella",
                        fluidPage(
                          tabsetPanel(
                            tabPanel("Time Series Plots", br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var1a", 
                                                     label = "Hourly Outdoor",
                                                     choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                                 "AQI (US AQI)","Temp (degrees F)",
                                                                 "CO2 (ppm)","Humidity (%)"), 
                                                     selected = "PM2.5"),
                                         selectInput("var2a", 
                                                     label = "Hourly Indoor",
                                                     choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                                 "AQI (US AQI)","Temp (degrees F)",
                                                                 "CO2 (ppm)","Humidity (%)"), 
                                                     selected = "PM2.5"),
                                         selectInput("var3a", 
                                                     label = "15-min Outdoor",
                                                     choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                                 "AQI (US AQI)","Temp (degrees F)",
                                                                 "CO2 (ppm)","Humidity (%)"), 
                                                     selected = "PM2.5"),
                                         selectInput("var4a", 
                                                     label = "15-min Indoor",
                                                     choices = c("PM10 (ug/m^3)","PM2.5 (ug/m^3)",
                                                                 "AQI (US AQI)","Temp (degrees F)",
                                                                 "CO2 (ppm)","Humidity (%)"), 
                                                     selected = "PM2.5"),
                                         br()),
                                       mainPanel(plotlyOutput("plot2a"), 
                                                 plotlyOutput("plot2b"),
                                                 plotlyOutput("plot2c"), 
                                                 plotlyOutput("plot2d"),
                                                 plotlyOutput("plot2e"))
                                     )),
                            tabPanel("Map", br(),
                                     helpText(h4("Estrella, Torreon, Mexico")),
                                     br(),
                                     leafletOutput("map2", width="1000", height="1000"),
                                     br()),
                            tabPanel("Hourly Data Table: Outdoors", 
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         br(strong("Variables:")),
                                         br("p2 = PM2.5 (ug/m^3)"), 
                                         br("p1 = PM10 (ug/m^3)"), 
                                         br("co = CO2 (ppm)"), 
                                         br("p01 = AQI (US AQI)"), 
                                         br("tp = TEMP(degrees F)"),
                                         br("hm = Humidity (%)")),
                                       mainPanel(
                                         DT::dataTableOutput("table3"),
                                         br()))),
                            tabPanel("Hourly Data Table: Indoors", br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         br(strong("Variables:")),
                                         br("p2 = PM2.5 (ug/m^3)"), 
                                         br("p1 = PM10 (ug/m^3)"), 
                                         br("co = CO2 (ppm)"), 
                                         br("p01 = AQI (US AQI)"), 
                                         br("tp = TEMP(degrees F)"),
                                         br("hm = Humidity (%)")),
                                       mainPanel(
                                         DT::dataTableOutput("table4"),
                                         br()))),
                            )))
           )



# Define server logic for random distribution app ----
server <- function(input, output) {
  ## Zona ----
  # Create the map
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -103.44407032947036, lat = 25.52765886793797, zoom = 15 )
  }) 
  
  # Tables 
  output$table1 <- renderDataTable({
    ozona_hr
  })
  output$table2 <- renderDataTable({
    izona_hr
  })
  output$table3 <- renderDataTable({
    oestrella_hr
  })
  output$table4 <- renderDataTable({
    iestrella_hr
  })
  output$plot1a <- renderPlotly({
    yvar <- switch(input$var1,
                   "PM10 (ug/m^3)" = ozona_hr$p1,
                   "PM2.5 (ug/m^3)" = ozona_hr$p2,
                   "AQI (US AQI)" = ozona_hr$p01,
                   "CO2 (ppm)" = ozona_hr$co,
                   "Humidity (%)" = ozona_hr$hm,
                   "Temp (degrees F)" = ozona_hr$tp
    )
    plot1<- ggplotly(ggplot(ozona_hr, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Hourly Outdoor ", input$var1))+
      ylab(paste0(input$var1))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1b <- renderPlotly({
    yvar <- switch(input$var2,
                   "PM10 (ug/m^3)" = izona_hr$p1,
                   "PM2.5 (ug/m^3)" = izona_hr$p2,
                   "AQI (US AQI)" = izona_hr$p01,
                   "CO2 (ppm)" = izona_hr$co,
                   "Humidity (%)" = izona_hr$hm,
                   "Temp (degrees F)" = izona_hr$tp
    )
    plot2<- ggplotly(ggplot(izona_hr, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Hourly Indoor ", input$var2))+
      ylab(paste0(input$var2))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1c <- renderPlotly({
    yvar <- switch(input$var3,
                   "PM10 (ug/m^3)" = ozona$p1,
                   "PM2.5 (ug/m^3)" = ozona$p2,
                   "AQI (US AQI)" = ozona$p01,
                   "CO2 (ppm)" = ozona$co,
                   "Humidity (%)" = ozona$hm,
                   "Temp (degrees F)" = ozona$tp
    )
    plot3<- ggplotly(ggplot(ozona, aes(x=datetime, y=yvar))+
      geom_point(color = "red")+
      ggtitle(paste0("Instant Outdoor ", input$var3))+
      ylab(paste0(input$var3))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1d <- renderPlotly({
    yvar <- switch(input$var4,
                   "PM10 (ug/m^3)" = izona$p1,
                   "PM2.5 (ug/m^3)" = izona$p2,
                   "AQI (US AQI)" = izona$p01,
                   "CO2 (ppm)" = izona$co,
                   "Humidity (%)" = izona$hm,
                   "Temp (degrees F)" = izona$tp
    )
    plot4<- ggplotly(ggplot(izona, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Instant Indoor ", input$var4))+
      ylab(paste0(input$var4))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  ### Estrella ----
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -103.42271353500566, lat = 25.562758975144888, zoom = 15 )
  })

  
  # Tables 
  output$table1 <- renderDataTable({
    oestrella_hr
  })
  output$table2 <- renderDataTable({
    iestrella_hr
  })
  output$table3 <- renderDataTable({
    oestrella_hr
  })
  output$table4 <- renderDataTable({
    iestrella_hr
  })
  output$plot2a <- renderPlotly({
    yvar <- switch(input$var1a,
                   "PM10 (ug/m^3)" = oestrella_hr$p1,
                   "PM2.5 (ug/m^3)" = oestrella_hr$p2,
                   "AQI (US AQI)" = oestrella_hr$p01,
                   "CO2 (ppm)" = oestrella_hr$co,
                   "Humidity (%)" = oestrella_hr$hm,
                   "Temp (degrees F)" = oestrella_hr$tp
    )
    plot5<- ggplotly(ggplot(oestrella_hr, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Hourly Outdoor ", input$var1a))+
      ylab(paste0(input$var1a))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2b <- renderPlotly({
    yvar <- switch(input$var2a,
                   "PM10 (ug/m^3)" = iestrella_hr$p1,
                   "PM2.5 (ug/m^3)" = iestrella_hr$p2,
                   "AQI (US AQI)" = iestrella_hr$p01,
                   "CO2 (ppm)" = iestrella_hr$co,
                   "Humidity (%)" = iestrella_hr$hm,
                   "Temp (degrees F)" = iestrella_hr$tp
    )
    plot6<- ggplotly(ggplot(iestrella_hr, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Hourly Indoor ", input$var2a))+
      ylab(paste0(input$var2a))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2c <- renderPlotly({
    yvar <- switch(input$var3a,
                   "PM10 (ug/m^3)" = oestrella$p1,
                   "PM2.5 (ug/m^3)" = oestrella$p2,
                   "AQI (US AQI)" = oestrella$p01,
                   "CO2 (ppm)" = oestrella$co,
                   "Humidity (%)" = oestrella$hm,
                   "Temp (degrees F)" = oestrella$tp
    )
    plot1<- ggplotly(ggplot(oestrella, aes(x=datetime, y=yvar))+
      geom_point(color = "red")+
      ggtitle(paste0("Instant Outdoor ", input$var3a))+
      ylab(paste0(input$var3a))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2d <- renderPlotly({
    yvar <- switch(input$var4a,
                   "PM10 (ug/m^3)" = iestrella$p1,
                   "PM2.5 (ug/m^3)" = iestrella$p2,
                   "AQI (US AQI)" = iestrella$p01,
                   "CO2 (ppm)" = iestrella$co,
                   "Humidity (%)" = iestrella$hm,
                   "Temp (degrees F)" = iestrella$tp
    )
    plot1<- ggplotly(ggplot(iestrella, aes(x=datetime, y=yvar))+ 
      geom_point(color = "red")+
      ggtitle(paste0("Instant Indoor ", input$var4a))+
      ylab(paste0(input$var4a))+
      xlab("Datetime")+
      theme_minimal()+ 
      theme(plot.title = element_text(face = "bold", size = 18))+
      theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  
}


# Run app ----
shinyApp(ui, server)
