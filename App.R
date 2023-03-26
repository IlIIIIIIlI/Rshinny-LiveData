if (!require(rstudioapi)) install.packages('rstudioapi')
# SET YOUR WORKING DIR
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# install library for you, dear user
if (!require(plotly)) install.packages('shiny')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(devtools)) install.packages('devtools')
if (!require(htmlwidgets)) install.packages('htmlwidgets')
if (!require(dplyr)) install.packages('dplyr')
if (!require(DT)) install.packages('DT')
if (!require(maps)) install.packages('maps')
if (!require(echarts4r)) install.packages('echarts4r')
# remotes::install_github('JohnCoene/echarts4r.maps')
if (!require(echarts4r.maps)) install.packages('echarts4r.maps')
if (!require(gapminder)) install.packages('gapminder')
if (!require(igraph)) install.packages('igraph')
if (!require(networkD3)) install.packages('networkD3')
if (!require(echarts4r)) install.packages('echarts4r')
if (!require(spdplyr)) install.packages('spdplyr')
if (!require(geojsonio)) install.packages('geojsonio')
if (!require(geojsonsf)) install.packages('geojsonsf')
if (!require(scales)) install.packages('scales')
if (!require(mapdeck)) install.packages('mapdeck')
if (!require(plotly)) install.packages('plotly')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(glue)) install.packages('glue')
if (!require(tibble)) install.packages('tibble')
# if (!require(ggiraph)) install.packages('ggiraph')
if (!require(ggiraphExtra)) install.packages('ggiraphExtra')
if (!require(waiter)) install.packages('waiter')
if (!require(echarts4r)) install.packages('echarts4r')


# devtools::install_github("Bart6114/sparklines")
# devtools::install_github('thomasp85/gganimate')
# devtools::install_github("ricardo-bion/ggradar",
#                          dependencies = TRUE)
# devtools::install_github("cardiomoon/ggiraphExtra")
# remotes::install_github("dreamRs/shinypop")

# to load shiny
library(shiny)
library(shinythemes)
library(htmlwidgets)
library(sparklines)
library(dplyr)
library(DT)

# map
library(maps)
library(echarts4r)
library(echarts4r.maps)

#animate 
library(gganimate)
library(gapminder)


# Libraries
library(igraph)
library(networkD3)

# library package for data wrangling both datfram and spatial
library(spdplyr)
library(geojsonio)
library(geojsonsf)
library(scales)

#library package for visualization
library(mapdeck)
library(plotly)
library(ggplot2)
library(glue)

# graphs
library(ggradar)
library(tibble)
require(ggiraph)
require(ggiraphExtra)

# the waiter - the loading page
library(waiter)
waiting_screen <- tagList(
  spin_pulsar(),
  h4("What the Bushfires have brought us...")
) 

# report library
library(shinypop)

########################
#   Data preprocessing #
########################
# the big three widges data
land_loss_value <- "Burned\n8600000\nhectares land"
animal_loss_value <- "Killed\n5000000000\nanimals"
money_loss_value <- "Loss\n23000000000\ndollars"

liquid <- data.frame(name= c(land_loss_value, 0.9, 0.7, 0.5), color = c("#BC3908", "#F6AA1C"))
liquid1 <- data.frame(name= c(animal_loss_value, 0.7, 0.4, 0.3), color = c("#621708", "#941B0C"))
liquid2 <- data.frame(name= c(money_loss_value, 0.7, 0.4, 0.1), color = c("#220901", "#F6AA1C"))

# radar graph data
radar_data <- read.csv(file='radar_norm.csv')

# 3d in the main page
# dealing with 3d data
# homeless data
aus_homeless <- read.csv(file='aus_homeless.csv')
map_3d <- read.csv(file='3dmap.csv')
matrix <- data.frame(
  x = rnorm(map_3d$year),
  y = rnorm(map_3d$month),
  z = rnorm(map_3d$homeless),
  color = map_3d$sick,
  size = map_3d$injured,
  stringsAsFactors = FALSE
) %>% 
  # group by by year and month
  dplyr::group_by(x, y) %>%
  # reduces multiple values down to a single summary
  dplyr::summarise(
    z = sum(z),
    color = sum(color),
    size = sum(size)
  ) %>%
  dplyr::ungroup()
print(matrix)

#####################
#  Graph data input #
#####################
fire_2019 <- read.csv(file='satellite_fire_2019.csv')
temperature_2019 <-  read.csv(file='temperature_2019.csv')
weather_2019 <-  read.csv(file='weatherAUS_2019.csv')
# set names for sparkline below
c_name <- setNames(as.character(unique(weather_2019$Location)), unique(weather_2019$Location))
head(weather_2019)


###############################
#   Interactive map component #
###############################
# the map in the main page
#API for the mapbox
key <- 'pk.eyJ1IjoiaGFyZHlzZXR5b3NvIiwiYSI6ImNrZXV6NTF0czFrZGQyeG4wNDVvZnczODgifQ.oy-_jwdY5W6PeQeUuo-6CQ'

# initializing the base map
draw_base_map <- function(){
  mapdeck(
    token = key,
    width = 300,
    height = 1000,
    style = "mapbox://styles/mapbox/dark-v10",
    pitch = 10,
    bearing = 10,
    zoom = 3,
    # the initial view of australia, i tried a lots of times
    location =c(-220.99701041459296,-25.716870845525336))
}


update_map <- function(mapdeck, sf, v) {
  
  m <- mapdeck_update(map_id = mapdeck)
  
  if ( v == "Bushfires in 2019" ) {
    m %>%
      # clean the canvas of last selection
      clear_hexagon(layer_id = "Temperature Distribution") %>%
      add_title(title = list(
        title = "Bushfires in 2019",
        css = "background-color: black;")) %>%
      add_hexagon(
        layer_id = "Bushfires",
        data = fire_2019,
        lat = "latitude",
        lon = "longitude",
        radius = 8000,
        elevation = "brightness",
        elevation_scale = 100,
        colour = "brightness",
        update_view = FALSE,
        legend = TRUE,
        colour_range = colourvalues::color_values(1:6, palette = 'plasma')
      )
  } 
  
  # selection bar for the temperature
  # what is the temperature like in 2019?
  else if ( v == "Temperature Distribution in 2019"){
    m %>%
      # clean the canvas of last selection
      clear_hexagon(layer_id = "Bushfires") %>%
      add_title(title = list(
        title = "Temperature in 2019",
        css = "background-color: black;")) %>%
      # the temperature chart is a scatter plot, so that it can have a nice tooltip
      add_scatterplot(
        data = temperature_2019,
        lat = "lat",
        lon = "long",
        layer_id = "Temperature Distribution",
        radius = 20000,
        fill_colour = "Temp",
        legend = TRUE,
        legend_format = list(fill_colour=as.integer),
        update_view = FALSE,
        tooltip = 'tooltip',
        palette = "inferno",
      )
    
  }
}

###############################
#   Word network component #
###############################
# create a dataset:
# word network
data <- read.csv('wordnet.csv')
p <- simpleNetwork(data, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   # distance between node.
                   linkDistance = 10,
                   charge = -900,
                   # size of the node names
                   fontSize = 14,               
                   fontFamily = "serif",      
                   linkColour = "#666",        
                   nodeColour = "#69b3a2",     
                   opacity = 0.9,              
                   zoom = T                    
)
print(data)

##################
# Shinny App Part#
##################
shinyApp(
  ##################
  # USER INTERFACE #
  ##################
  ui =
    # main page design
    navbarPage(
      # choose a theme for shiny app
      theme = shinythemes::shinytheme("cyborg"),
      # title
      "Bear in Bushfires",
      # 1111111111111111111111111111111111111111111111111111111111
      # first panel is about the basic introduction of bushfires
      tabPanel("Fact",
               fluidPage(
                 # loading animation
                 useWaiter(), 
                 waiterShowOnLoad(html = waiting_screen, color="black"),
                 
                 h4("Bushfires Traceborad", style="text-align:center"),
                 br(),
                 h6(
                   "More than 5 million hectares have been consumed by unprecedented bushfires, which have also claimed the lives of nine individuals."
                 ),
                 h6(
                   " Approximately 3.4 million hectares have been burned in NSW alone. It is believed that between 15,000 and 28,000 koalas inhabit the north central coast of the state."
                 ),
                 div("Up to 30 percent of the region's koalas have perished due to the destruction of up to 30 percent of their habitat. Other endangered and fragile creatures, such as the Western Ground Parrot in Western Australia and the Kangaroo Island Dunnart (a tiny marsupial) in South Australia, have also been harmed by the fires.  Climate change is a significant issue, and we are doing our best to combat it. We are meeting and exceeding our objectives, which is crucial. with concrete actions in Australia that make a difference."),
                 br(),
                 br(),
                 fluidRow(
                   column(
                     width = 2,
                     align="center",
                     fluidRow(
                       # widges that showing give a brief dashboard intro, the first one 
                       liquid %>% 
                         # define the charts
                         e_charts(height=220, width=220) %>% 
                         # define the text
                         e_text_style(
                           color = "#D94854",
                           fontStyle = "italic",
                           fontSize = 2
                         )  %>% 
                         # define the liquid itself
                         e_liquid(
                           serie = name,
                           shape = 'roundRect',
                           color = color,
                           # the format of text inside the circle
                           label = list(
                             formatter = "{c}",
                             fontSize = 13
                           ),
                           outline = list(
                             borderDistance= 0,
                             itemStyle= list(
                               borderWidth= 5,
                               borderColor= '#156ACF',
                               shadowBlur= 20,
                               shadowColor= 'rgba(255, 0, 0, 1)'
                             )
                           )
                         )
                     ),
                   ),
                   # the second widget
                   column(
                     width = 2,
                     align="center",
                     liquid1 %>% 
                       e_charts(height=220, width=220) %>% 
                       e_text_style(
                         color = "#D94854",
                         fontStyle = "italic",
                         fontSize = 2
                       )  %>% 
                       e_liquid(
                         serie = name,
                         shape = 'roundRect',
                         color = color,
                         label = list(
                           formatter = "{c}",
                           fontSize = 13
                         ),
                         outline = list(
                           borderDistance= 0,
                           itemStyle= list(
                             borderWidth= 5,
                             borderColor= '#156ACF',
                             shadowBlur= 20,
                             shadowColor= 'rgba(255, 0, 0, 1)'
                           )
                         )
                       ),
                   ),
                   # the third widget
                   column(
                     width = 2,
                     align="center",
                     liquid2 %>% 
                       e_charts(height=220, width=220) %>% 
                       e_text_style(
                         color = "#D94854",
                         fontStyle = "italic",
                         fontSize = 2
                       )  %>% 
                       e_liquid(
                         serie = name,
                         shape = 'roundRect',
                         color = color,
                         label = list(
                           formatter = "{c}",
                           fontSize = 13
                         ),
                         outline = list(
                           borderDistance= 0,
                           itemStyle= list(
                             borderWidth= 5,
                             borderColor= '#156ACF',
                             shadowBlur= 20,
                             shadowColor= 'rgba(255, 0, 0, 1)'
                           )
                         )
                       )
                   ),
                   # the 3-d map
                   column(
                     width = 6,
                     # so that it can fit itself
                     align="center",
                     matrix %>% 
                       # fixed the graph size
                       e_charts(x,height=360, width=360) %>% 
                       # create the 3d map
                       e_scatter_3d(y, z, size, color, symbol='pin') %>% 
                       # # the 3d map adjusting tool, controlling the size
                       e_visual_map(
                         right = 15,     # dist between components and page left
                         bottom = 20,   # dist between components and page below
                         size,
                         inRange = list(symbolSize = c(1, 30)), # scale size
                         dimension = 3, # third dimension 0 = x, y = 1, z = 2, size = 3
                         text = c("Max Injuries", "Min Injuries"),
                         # map components text style
                         textStyle = list(
                           color = "#fff"
                         ),
                       ) %>%
                       # the 3d map adjusting tool 2
                       e_visual_map(
                         up = 40,     # dist between components and page left
                         bottom = 20,   # dist between components and page below
                         color,
                         # red color represents the cruel truth
                         inRange = list(color = c('#03071e', '#6a040f','#d00000','#dc2f02','#e85d04','#f48c06', '#faa307', '#ffba08')), # scale colors
                         dimension = 4,
                         text = c("Max Sick", "Min sick"),
                         # map components text style
                         textStyle = list(
                           color = "#fff"
                         )
                       ) %>%
                       e_title("Animals' Degree of victimization", "X axis represents Year (2017-2019)\nY axis represents Month\nZ axis represents Homeless\nAll data normed")
                   )
                 ),
                 
                 # this is the selector for the main page map
                 h6(HTML("Does average temperature affect bushfires severity? See the comparison.")),
                 selectInput("viz","",
                             choices = c("Bushfires in 2019","Temperature Distribution in 2019"),
                             selected = "Bushfires in 2019"),
                 br(),
                 
                 # MAP
                 # this is the main page map
                 mapdeckOutput("mapdeck"),
                 br(),
                 br(),
                 # a movable panel, might have something useful function scenarios
                 fluidRow(
                   
                 ),
                 h6("Bushfires Traceborad", style="text-align:center"),
               )
               
               
               
      ),
      # 22222222222222222222222222222222222222222222222222222222222
      # the second panel, descriping the impact of human and koalas
      tabPanel("Impact",
               fluidPage(
                 h4(
                   "Attention and action in response to explicit and implicit impacts", style="text-align:center")
                 ,
                 br(),
                 h6(
                   "people absolutely moved and amazed by, number one, our wildlife volunteer response, and also by the habits of these curious creatures. Bushfires destroying thousands of hectares of koala's forests. So, what is the future for humans and koalas? "
                   , style="text-align:center"),
                 br(),
                 fluidRow(
                   column(
                     # some describe text of the twitter network
                     width=6,
                     align="right",
                     
                     h2("\u279C"),
                     h6("Voices of the mainstream media."),
                     div("As we go from 2019 into 2022, our aspirations for rainy days remain unfulfilled."),
                     div("This ecological catastrophe is equally tragic. Individuals will be required to aid themselves rather than others. Greenpeace has started a fundraising effort to raise A$75,000 (HK$400,000), more than the federal government's funding for an entire year, for the NSW Rural Fire Service to help the professional teams on the front lines of the flames and to keep people and forests safe."),
                     div("On the other hand, big cities like as Sydney and Brisbane have not been spared the public health problem created by air pollution, and Greenpeace is distributing masks in core business districts to enhance public awareness of the fires and protection."),
                     h6("Extreme drought was inevitable for this reason."),
                     div("The drought in New South Wales since 2017 and the driest January to August on record in portions of South Australia in 2017 have created a combustible tinderbox, and the early start of the August 2018 fire season is a wake-up call, according to experts."),
                     div("Lack of federal support, as well as significant budget cuts to regional fire brigades, prevented early hazardous burning reduction (pre-emptive removal of weeds and brush from high-risk hotspots under controlled fire conditions"),
                     div("some preventative measures were ineffective due to dry conditions), which made vegetation more flammable and accelerated the spread of fires."),
                     div("Obviously, understanding of the weather also aids in preventing potential disasters."),
                     h2("\u21CA")
                     
                   ),
                   column(
                     width=6,
                     align="center",
                     p
                   )
                 ),
                 fluidRow(
                   column(
                     width = 1,
                     align="center",
                     div("Location",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     textOutput("location"),
                     tags$head(tags$style("#location{color: #F6AA1C;
                                 font-size: 12px;
                                 }"
                     ))
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Rainfall",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Rainfall")),
                   ),
                   column(
                     width = 1,
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Wind",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Wind")),
                   ),
                   column(
                     width = 1,
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Humidity9am",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Humidity9am")),
                   ),
                   column(
                     width = 1,
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Humidity3pm",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Humidity3pm")),
                   ),
                   column(
                     width = 1,
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Temp9am",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Temp9am")),
                   ),
                   column(
                     width = 1,
                   ),
                   column(
                     width = 1,
                     align="center",
                     div("Temp3am",style="color:#F6AA1C;font-size:17px;font-style: italic"),
                     mainPanel(sparklineOutput("spark_Temp3am")),
                   )
                 ),
                 br(),
                 # my sparkline panel
                 # the column width 1 make it more looks good
                 fluidRow(
                   sidebarPanel(
                     dateRangeInput("weather_dates", "Weather Dashboard Date Range",
                                    start = min(weather_2019$Date),
                                    end = max(weather_2019$Date)),
                     selectInput("locations", "Location",choices = c_name),
                     # the sparkline functions, can be more charts
                     selectInput("chart_type", "Chart type",choices = list("line","bar","box")),
                   ),
                 ),
                 br(),
                 fluidRow(
                   div("CourseCode:GEOM90007; StudentName: Quechen YANG; StudentNumber: 1230851", style="text-align:center")
                 ),
                 fluidRow(
                 ),
                 fluidRow(
                 )
               )
      ),
      
      
      # 33333333333333333333333333333333333333333333333333333333333
      # the third panel, descriping the what we can do
      tabPanel("Act",
               fluidPage(
                 h4(
                   "Start with a Fast Act from you and me", style="text-align:center")
                 ,
                 br(),
                 h6(
                   "The fact they are popping up in these areas where they are just not common is a good sign they are out there and expanding."
                   , style="text-align:center"),
                 br(),
               div(
                 "Habitat destruction, bushfires, traffic accidents and chlamydial infections have all contributed to the decline.   Chlamydia can cause blindness, cysts in the koala's reproductive tract can lead to sterility and in some cases even death.   Antibiotics used to treat this disease can destroy the koala's intestinal tract, causing some to starve to death even after they have been cured. More than 80 per cent of the World Heritage-listed Greater Blue Mountains Area was burnt during the 2019/20 bushfires, sparking fears unmapped koala populations had been destroyed."
               ),
               br(),
               br(),
               fluidRow(
               # the dashboard part in panel3
               column(
                 # radar goes to right
                 width=3,
                 align="center",
                 # render the radar plot
                 ggRadar(
                   data=radar_data,  # Simply the size of the point 
                   aes(color=Year),
                   interactive=TRUE,
                   size = 3
                 )
               ),
               column(
                 width=9,
                 align="center",
                 # visualize how many people lost their home.
                 aus_homeless |>
                   # use for timeline, groupby
                   group_by(date) |>
                   # initialize charts
                   e_charts(x = state_name, timeline = TRUE) |>
                   em_map(map = "Australia") |>
                   # use the embedding australia map
                   e_map(
                     serie = homeless_service, map = "Australia", name = "Number of homeless people get helped"
                   ) |>
                   # map text style
                   e_text_style(
                     color = "#FF0000",
                     fontStyle = "italic",
                     fontSize = 12
                   ) |>
                   # map components
                   e_visual_map(
                     serie = homeless_service,
                     left = 400,     # dist between components and page left
                     bottom = 60,   # dist between components and page below
                     text = c("Max Homeless", "Min Homeless"),
                     # map components text style
                     textStyle = list(
                       color = "#fff"
                     ),
                     # map colour
                     inRange = list(
                       color = RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")
                     )
                   ) |>
                   e_tooltip() |>
                   e_timeline_opts(
                     autoPlay = TRUE,    # autoplay
                     show = TRUE,        # show the timeline
                     playInterval = 1000 # turn time
                   ) |>
                   # control the timeline series
                   e_timeline_serie(title = lapply(
                     unique(aus_homeless$date),
                     function(date) {
                       list(
                         text = paste("Specialist Homelessness Services in ", date),
                         subtext = "Data source: Australian Institute of Health and Welfare",
                         sublink = "https://www.aihw.gov.au/reports-data/health-welfare-services/homelessness-services/data?page=1",
                         left = "center",
                         textStyle = list(
                           color = "#F6AA1C",
                           fontWeight = "bolder"
                         ),
                         subtextStyle = list(
                           color = "#fff"
                         )
                       )
                     }
                   ), index = 1)
               )),
               br(),
               br(),
               h5(
                 "Currently only the database of koala sightings in Queensland is complete. If you too have spotted a koala, don't hesitate to use this tool to record the location."
               ),
               br(),
               br(),
               use_notiflix_confirm(),
               tags$h4("I need to report a koala sighting!"),
               textInput("caption", "Following format: Time-Location-Status", "e.g. 2022-09-26  University of Melbourne A Koala is eating leaf",width=500),
               actionButton("show", "Report it!"),
      )
      
      )
      
    )
  ,
  
  ################
  # SHINY SERVER #
  ################
  server = function(input, output) {
    # for rendering the map in the main page
    map_base <- reactive({
      draw_base_map()
    })
    
    # for output the main page map
    output$mapdeck <- renderMapdeck({
      map_base()
    })
    sf_reactive <- reactive({
      choose_obs(input$viz)
    })
    observe({
      update_map("mapdeck", sf_reactive(), input$viz)
    })
    
    #output the second page's sparkline
    output$location <- renderText({
      paste(input$locations)
    })
    # use dataframe filter to return the corresponding information
    output$spark_Rainfall <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$Rainfall, input$chart_type)
    })
    output$spark_Wind <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$WindGustSpeed, input$chart_type)
    })
    output$spark_Humidity9am <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$Humidity9am, input$chart_type)
    })
    output$spark_Humidity3pm <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$Humidity3pm, input$chart_type)
    })
    output$ spark_Temp9am <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$Temp9am, input$chart_type)
    })
    output$spark_Temp3am <- renderSparkline({
      sparkline(weather_2019[weather_2019$Date >= input$weather_dates[1] & weather_2019$Date <= input$weather_dates[2] & weather_2019$Location == input$locations,]$Temp3pm, input$chart_type)
    })
   
    # wait til all process have done
    Sys.sleep(5)
    waiter_hide()
    
    # report panel output
    observeEvent(input$show, {
      nx_confirm(
        inputId = "confirm",
        title = "Do you want to confirm?",
        button_ok = "Sure!",
        button_cancel = "Nope!"
      )
    })
  }
)

install.packages('rsconnect')
rsconnect::setAccountInfo(name='chenoi', token='11BEBF54F8781CEC4A26F42B08291D4C', secret='IgKrjkEdhzY2CRxHODPVsjUy0Tqrg2o5wUWIzQ7Q')
library(rsconnect)
rsconnect::deployApp(appName="ChenoiLAB-LoveKoala")


