# Project 1 for CS 424 Spring 2018 UIC - Bartosz Kupiec

#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape2)
library(scales)
require(dplyr)

# assume all data is inside a directory called data
deaths <- read.table(file = "data/choleraDeaths.tsv", header = TRUE)
ageAndSex <- read.table(file = "data/naplesCholeraAgeSexData.tsv", header = TRUE)
census <- read.table(file = "data/UKcensus1851.csv", header = TRUE,sep = ',')
deathLocations <- read.table(file = "data/choleraDeathLocations.csv", header = FALSE,sep = ',')
pumpLocations <- read.table(file = "data/choleraPumpLocations.csv", header = FALSE,sep = ',')

#add totals for the census data
census$Total = census$female + census$male


#make sure death information is filled completly and correctl
complete.cases(deaths) 
deaths[complete.cases(deaths), ]
deaths <- deaths[complete.cases(deaths), ]
paste(deaths$Attack + deaths$Death)

#make sure input is seen as integer values 
deaths$Attack <- as.integer(deaths$Attack)
deaths$Death <- as.integer(deaths$Death)

#create a new field in the table for the totals 
deaths$TotalDeaths <- cumsum(deaths$Death)
deaths$TotalAttacks <- cumsum(deaths$Attack)

#format the date field properly 
deaths$Date <- as.Date(deaths$Date, format = "%d-%b-%Y")
deaths$Date <- format(as.Date(deaths$Date), format = "%m-%d-%Y")

#format the death locations as long/lat, and add new collumn 
deathLocations$type = "death"
pumpLocations$type = "well"

colnames(deathLocations) <- c("deaths", "longitude","latitude","type" )
colnames(pumpLocations) <- c("deaths", "longitude","latitude","type" )

joined <- rbind(deathLocations,pumpLocations)

# start up the gui 
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 1"),
  dashboardSidebar(disable = TRUE),
  
  #start of the body 
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 240px) !important;}"),
    tabsetPanel( 
      tabPanel("Deaths and attacks",
        
        fluidRow(
        box( title = "Deaths in 1854 from the London cholera outbreak", solidHeader = TRUE, status = "primary", width = 10,
             plotOutput("graph0"))
      ),
      box(title = "Deaths and Attacks ", solidHeader = TRUE, status = "primary", width = 10,
          dataTableOutput("tab1")
        )
      ),
      tabPanel("Age/Sex", 
               
      fluidRow(
        box(title = "Males vs Females fatalities (Thousands)", solidHeader = TRUE, status = "primary", width = 4,
            dataTableOutput("tab2")),
        box( title = "Male fatalities (Thousands)", solidHeader = TRUE, status = "primary",width = 3,
             plotOutput("hist0")),
        box( title = "Female fatalities (Thousands)", solidHeader = TRUE, status = "primary",width = 3,
             plotOutput("hist1"))
      )),
      tabPanel("Census data",
               fluidRow(
                 box(title = "Census data (United Kingdom 1851) ", solidHeader = TRUE, status = "primary",width = 3,
                     dataTableOutput("tab3")),
                 box( title = "Males", solidHeader = TRUE, status = "primary", width = 4,
                      plotOutput("censusBar0")),
                 box( title = "Females", solidHeader = TRUE, status = "primary",width = 4,
                      plotOutput("censusBar1")),
                 box( title = "Males", solidHeader = TRUE, status = "primary", width = 3,
                      plotOutput("bar0")),
                 box( title = "Females", solidHeader = TRUE, status = "primary",width = 3,
                      plotOutput("bar1")),
                 box( title = "Males vs Females", solidHeader = TRUE, status = "primary", width = 3,
                      plotOutput("bar2"))
               )  
               
               
      ),
      
      tabPanel("Death/pumps locations", 
      box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
          leafletOutput("map")
      )),
      tabPanel("About " , icon = icon("info-sign", lib = "glyphicon"),
               
               column(11,
                      pre(includeText("include.txt"))
               )
               )
      
    )
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_dark(base_size = 18))
  
  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$tab1 <- DT::renderDataTable(
    DT::datatable({ 
      deathsTable <-  deaths
    }, 
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
                   
                   ) 
    )
  )

  
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
      ageAndSexTable <-  ageAndSex
      }, 
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 9, lengthChange = FALSE,dom = 't', order = list(list(0, 'asc'))
                   
    ) 
    )
  )
  
  output$tab3 <- DT::renderDataTable(
    DT::datatable(census, 
                  options = list(searching = FALSE,dom = 't',pageLength = 9, lengthChange = FALSE,
                                 columnDefs = list(list(className = 'dt-left', 
                                                        targets = 0:4)))) %>% 
      formatCurrency(2:4, ''),class = 'cell-border stripe'
  )
  
  
#graph outputs 
  output$graph0 <- renderPlot({
    my_data <- melt(deaths, id ="Date")
    
    ggplot(my_data, aes(Date, value,colour = variable,group=variable)) +
      geom_line() +   
      geom_point(size = 1) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 0)) 
 })
  
  output$hist0 <- renderPlot({
    ggplot(ageAndSex, aes(age,male)) +
      geom_text(
        aes(label = male),
        position = position_dodge(0.9),
        vjust = -0.5
      ) +
      geom_bar(stat="identity", fill="steelblue")  +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      ylab("Deaths (Thousands)") +
      xlab("Age group")
  })
  
  output$hist1 <- renderPlot({
    ggplot(ageAndSex, aes(age,female)) +
      geom_text(
        aes(label = female),
        position = position_dodge(0.9),
        vjust = -0.5
      ) +
      geom_bar(stat="identity", fill="steelblue")  +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      ylab("Deaths (Thousands)") +
      xlab("Age group")
  })
  
  
  output$bar0 <- renderPlot({
    ggplot(census, aes(x ="",male,fill = age)) +
      geom_bar(width = 3, stat="identity")  +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Dark2") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
    theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
  })
  
  output$censusBar0 <- renderPlot({
    ggplot(census, aes(age,male)) +
      geom_text(
        aes(label = comma(male)),
        position = position_dodge(0.9),
        vjust = -0.5
      ) +
      geom_bar(stat="identity", fill="steelblue")  +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      ylab("Males") +
      xlab("Age group")
  })
  
  output$censusBar1 <- renderPlot({
    ggplot(census, aes(age,female)) +
      geom_text(
        aes(label = comma(female)),
        position = position_dodge(0.9),
        vjust = -0.5
      ) +
      geom_bar(stat="identity", fill="steelblue")  +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      ylab("Females") +
      xlab("Age group")
  })
  
  
  output$bar1 <- renderPlot({
    ggplot(census, aes(x ="",female,fill = age)) +
      geom_bar(width = 1, stat="identity")  +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Dark2") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
  })
  
  output$bar2 <- renderPlot({
 
    combinedDeaths <- melt(census, id = "age")
    
    totalMale <- cumsum(census$male)
    totalFemale <- cumsum(census$female)
    
    myDF <- data.frame(
      group = c("Male", "Female"),
      value = c(totalMale, totalFemale)
    )
    
    ggplot(myDF, aes(x ="",value,fill = group)) +
      geom_bar(width = 1, stat="identity")  +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Dark2") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
      
  })
  
  # add a leaflet map and put markers where the deaths occured

  output$map <- renderLeaflet({
    pal <- colorFactor(c("red","blue"), domain = c("well","death"))
    
    m <-leaflet(joined) %>% addTiles() %>% addCircleMarkers(
        radius = ~ifelse(type == "well", 4, joined$deaths + 1),
        color = ~pal(type),
        stroke = FALSE,
        fillOpacity = ~ifelse(type == "well", 1, .5),
        label = ~ifelse(type == "death",paste("Deaths:", joined$deaths) , "Well")
        ) 
    # use the black/white map so it doesn't colide with the data we are displaying 
    m = addProviderTiles(map = m, provider = "CartoDB.Positron")
    m <- setView(m, lng = -0.136668,lat = 51.513341 , zoom = 16)
    m
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Information",
      "Created by Bartosz Kupiec.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}


shinyApp(ui = ui, server = server)