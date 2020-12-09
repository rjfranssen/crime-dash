#############################################################
# Load libraries
# Ref: https://rstudio.github.io/shinydashboard/structure.html
#############################################################
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(ggplot2)
library(maps)
#library(ggmap)
#library(mapproj)
library(dplyr)
library(tidyr)
library(sqldf)
library(scales)
library(plotly)
library(data.table)
states_map <- map_data("state")

#############################################################
# Data Preprocessing
#############################################################

# CRIMES DATASET
crimes_df <- read.csv('crimes_df.csv')
crimes_df$state <- tolower(crimes_df$state)

# # read data set and the shapes for drawing states from map package then create a data set where state names and order match mapStates. 
# mapStates = map("state", fill = TRUE, plot = FALSE)
# 
# snames_c <- data.frame(mapStates$names)
# colnames(snames_c) <- c("state")
# snames_c$state <- gsub(":.*", "", snames_c$state) 
# npop_c <- merge(snames_c, crimes_df, by="state")
# 
# stcenters <- data.frame(tolower(state.name), state.center)
# colnames(stcenters) <- c("state","Lon","Lat")
# snpop_c <- merge(npop_c, stcenters, by="state")

#gnpop = gather(npop, State)
#colnames(gnpop) <- c("State", "Year", "Pop")
#gnpop_c = gather(npop_c, Year, Pop, -State)

metrics_list <- c('state', 'population', 'violent_crime', 'murder_and_manslaughter', 'rape_legacy', 'rape_revised', 'robbery', 'aggravated_assault', 'property_crime', 'burglary', 'larceny_theft', 'mvt', 'violent_crime_rate', 'murder_manslaughter_rate', 'rape_legacy_rate', 'rape_revised_rate', 'robbery_rate', 'aggravated_assault_rate', 'property_crime_rate', 'burglary_rate', 'larceny_theft_rate', 'mvt_rate', 'year')

##################
# TEST DATASET
##################
# read data set and the shapes for drawing states from map package then create a data set where state names and order match mapStates. 
pop <- read.csv('pop_state_proj.csv')
pop$State <- tolower(pop$State)
pal <- colorNumeric("YlOrRd", domain = c(0,47000000))
mapStates = map("state", fill = TRUE, plot = FALSE)

snames <- data.frame(mapStates$names)
colnames(snames) <- c("State")
snames$State <- gsub(":.*", "", snames$State) 
npop <- merge(snames, pop, by="State")

stcenters <- data.frame(tolower(state.name), state.center)
colnames(stcenters) <- c("State","Lon","Lat")
snpop <- merge(npop, stcenters, by="State")

#gnpop = gather(npop, State)
#colnames(gnpop) <- c("State", "Year", "Pop")
gnpop = gather(npop, Year,Pop, -State)


#############################################################
# UI Function
#############################################################
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "FBI Crime Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crimes Dash 1", tabName = "crimes-dash-1", icon=icon("dashboard")),
      menuItem("Crimes Dash 2", tabName = "crimes-dash-2", icon=icon("dashboard")),
      menuItem("Interactive Map", tabName = "interactive-map", icon=icon("globe")),
      menuItem("(Test Dash)", tabName = "test-dashboard-tab-id", icon=icon("dashboard"))
      #menuItem("Raw data", tabName = "rawdata-tab-id", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    #shinyDashboardThemes(
    #  theme = "grey_dark"
    #),
    tabItems(
      
      #############################################################
      # CRIMES DASH 2
      #############################################################
      tabItem(tabName="crimes-dash-1",
              fluidRow(
                column(4, selectInput(inputId="select_state_1", label="State: ", choices=snames, selected="pennsylvania")),
                column(4, selectInput(inputId="select_metric_1", label="Metric: ", choices=metrics_list, selected="mvt"))
              ),
              fluidRow(
                column(8, plotlyOutput('crimes_plot_1'))
              )
      ),
      #############################################################
      
      #############################################################
      # CRIMES DASH 2
      #############################################################
      tabItem(tabName="crimes-dash-2",
              fluidRow(
                #column(4, selectInput(inputId="select_state_2", label="State: ", choices=snames, selected="pennsylvania")),
                column(4, selectInput(inputId="select_metric_2", label="Metric: ", choices=metrics_list, selected="mvt"))
              ),
              fluidRow(
                column(8, plotlyOutput('crimes_plot_2'))
              )
      ),
      #############################################################
      
      #############################################################
      # TEST DASHBOARD
      #############################################################
      tabItem(tabName="test-dashboard-tab-id",
              fluidRow(
                #box(leafletOutput('map')),
                #box("Box content here", br(), "More box content",
                #    plotOutput('plot'),
                #    sliderInput("slider", "Slider input:", 1, 100, 50),
                #    textInput("text", "Text input:")
                #    ),
                column(6, leafletOutput('map')),
                column(6, plotOutput('plot'))
              ),
              fluidRow(column(4,verbatimTextOutput('text')), 
                       column(8, sliderInput("slider","Years", min=2000, max=2030, step=5, value=2000, sep="")))
      ),
      #############################################################
      
      #############################################################
      # INTERACTIVE MAP
      #############################################################
      tabItem(tabName="interactive-map",
              fluidRow(
                column(4,verbatimTextOutput('text_3')), 
                column(4, selectInput(inputId="select_metric_3", label="Metric: ", choices=metrics_list, selected="mvt")),
                column(4, sliderInput("slider_3","Year", min=1994, max=2012, step=2, value=2012, sep=""))
              ),
              
              fluidRow(
                column(8, leafletOutput('crimes_map_3'))
              ),
              
              fluidRow(
                column(8, plotOutput('crimes_plot_3'))
              )
      )
      #############################################################
    )
  
  )
)

#############################################################
# Server Function
#############################################################
server <- function(input, output) {
  
  #############################################################
  # CRIMES DASHBOARD
  #############################################################
  
  #############################################################################
  # 1. CRIMES BAR CHART SELECTABLE
  #############################################################################

  # Faceting by metric
  output$crimes_plot_1 <- renderPlotly({
    
    #plot_data_1 <- crimes_df %>% filter(state==input$select_state_1) %>% select(year, state, mvt)
    plot_data_1 <- crimes_df %>% filter(state==input$select_state_1) %>% select(year, state, input$select_metric_1)
    plot_data_long_1 <- melt(as.data.table(plot_data_1), id.vars = c('state', 'year'))
   
    plt_1 <- ggplot(plot_data_long_1, aes(x=year, y=value)) +
      #geom_boxplot(aes(group=year, color=year)) + 
      geom_bar(stat = "identity") +
      #geom_jitter(width = 0.2) +
      #facet_wrap(~variable, scales="free") +
      #facet_wrap(~year, scales="free") +
      #coord_flip() +
      ylab("Motor Vehicle Thefts") +
      #xlab("") +
      #ggtitle(paste0("Motor Vehicle Thefts in ", input$select_state_1, " per 100k People from 1994-2012")) +
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(plt_1)
    
  }) 
  
  #############################################################################
  # 2. CRIMES BAR CHART AVG BY STATE
  #############################################################################
  
  output$crimes_plot_2 <- renderPlotly({
    
  #options(repr.plot.width=12, repr.plot.height=8)
  
  plot_data_2 <- sqldf(paste0("select state, avg(", input$select_metric_2, ") as avg_rate from crimes_df group by 1 order by 1"))
  
  plt_2 <- ggplot(plot_data_2, aes(x=state, y=avg_rate)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(label=comma) +
    ggtitle(paste0("Average Rates of ", input$select_metric_2, " per 100k People from 1994-2012")) +
    ylab(paste0(input$select_metric_2)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  ggplotly(plt_2)
  
  })
  
  
  #############################################################################
  # 3. Interactive Map
  #############################################################################
  
  # INITIALIZE REACT VARIABLES
  reac_3 <- reactiveValues(year=1994, metric='mvt')
  
  # MAKE REAC DEPENDENT ON INPUT$SLIDER (update reac$year) 
  observe ({ 
    
    # if (input$slider_1 == "2000") {
    #   reac_1$year = paste0("Census_", input$slider_1) 
    # }
    # else {
    #   reac_1$year = paste0("Proj_",input$slider_1)
    # }
    
    reac_3$year = input$slider_3

    reac_3$metric = input$select_metric_3
    
  })
  
  # CREATE MAP OUTPUT (map)  
  output$crimes_map_3 = renderLeaflet({   
    
    # UPDATE THE MAP WHENEVER REAC$YEAR IS CHANGED     
    map_data <- crimes_df %>% filter(year==reac_3$year) %>% select(reac_3$metric)
    colnames(map_data) <- c('variable')
    pal_3 <- colorNumeric("YlOrRd", domain = c(min(map_data), max(map_data)))
    
    
    crimes_map_3 <- leaflet(data = mapStates) %>%
      addTiles() %>%
      
      # Add State Polygons - fillColor based on population for year selected
      #addPolygons(fillColor = ~pal(crimes_df %>% filter(year==reac_3$year) %>% select(reac_3$metric)),
      addPolygons(fillColor = ~pal_3(map_data$variable),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.9,
                    bringToFront = TRUE)
      )
    
    crimes_map_3
  })
  
 
  
  #############################################################
  # TEST DASHBOARD FOR POP DATASET
  #############################################################
  # INITIALIZE REACT VARIABLES
  reac <- reactiveValues (year = "Census_2000", state = "missouri")
  
  # MAKE REAC DEPENDENT ON INPUT$SLIDER (update reac$year) 
  observe ({ 
    
    if (input$slider == "2000") {
      reac$year = paste0("Census_", input$slider) 
    }
    else {
      reac$year = paste0("Proj_",input$slider)
    }
  })
  
  # CREATE MAP OUTPUT (map)  
  output$map = renderLeaflet({      
    
    # UPDATE THE MAP WHENEVER REAC$YEAR IS CHANGED     
    map <- leaflet(data = mapStates) %>% 
      addTiles() %>%
      
      # Add State Polygons - fillColor based on population for year selected
      addPolygons(fillColor = ~pal(npop[[reac$year]]), 
                  weight = 1, 
                  opacity = 1, 
                  color = "white", 
                  fillOpacity = 0.9, 
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.9,
                    bringToFront = TRUE)
      ) %>%
      
      # Add Circles - Radius based on population in year 2000 
      addCircles(~snpop$Lon, ~snpop$Lat, radius=~sqrt(snpop$Census_2000)*10,
                 color="orange",
                 stroke = TRUE, 
                 fillOpacity = 0.2,
                 popup = ~as.character(snpop$State)
      )
    
    map
  })
  
  # CREATE BARCHART OUTPUT (plot)
  output$plot = renderPlot({
    
    # UPDATE THE CHART WHENEVER REAC$STATE IS CHANGED
    p <- ggplot(data=subset(gnpop, State==reac$state), aes(x=Year, y=Pop, group=reac$state)) +
      # 1- The plot on the right should have line plots for all states grayed out as in the example app link.
      # 3- The annotations on the right plot should be prominently placed (font, size, etc.) like in the example.
      #geom_bar(stat="identity") + 
      geom_line(data=gnpop, aes(x=Year, y=Pop, group=State), color="lightgrey", size=1) + # All States
      geom_line(position='identity', size=3) + # SELECTED STATE Need to add 'group' to aesthetic for geom_line
      ylim(0, 47000000) +
      ggtitle(paste0(reac$state, " population projection"))
    
    p
  })
  
  # OBSERVE LEAFLET OBJECT EVENT CLICK
  # 2- When the user hovers over a state, the state's line on the right plot should be drawn in darker color.
  #observeEvent(input$map_shape_click, {
  observeEvent(input$map_shape_mouseover, {
    
    # GET THE COORDINATES FOR THE SHAPE CLICK EVENT 
    lon <- input$map_shape_mouseover$lng
    lat <- input$map_shape_mouseover$lat
    
    # USE MAP.WHERE TO FIND THE STATE NAME BASED ON THE CLICK COORDINATES ("maps" package)
    st <- map.where("state", lon, lat)
    
    # UPDATE REAC$STATE WITH THE CLICK EVENT - WILL TRIGGER PLOT UPDATE 
    # 4- At least: state's name and population should be displayed in the annotation.
    reac$state <- gsub(":.*", "", st)  # get everything until : character 
    output$text <- renderPrint(paste0("State: ", isolate(reac$state), " Population: ", gnpop[gnpop$State==reac$state & gnpop$Year==reac$year,]$Pop))
  })
}

#############################################################
# Start App
#############################################################
#shinyApp(ui=ui, server=server)
shinyApp(ui=ui, server=server, options = list(display.mode = 'showcase'))
# Ref for display modes: https://shiny.rstudio.com/articles/display-modes.html