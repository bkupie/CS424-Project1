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

#we will now make sure we have all the data we need to make the visualizations#

#make sure death information is filled completly and correct for the first Deaths and Attacks tab
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

  
#add totals for the census data
  census$Total = census$female + census$male
  
  
#format the death locations as long/lat, and add new collumn to keep track of the data
# is coming from the well file or the death file 
  deathLocations$type = "death"
  pumpLocations$type = "well"
  #rename the collumns to have the same names, and make sure it says long/lat for leafy map
  colnames(deathLocations) <- c("deaths", "longitude","latitude","type" )
  colnames(pumpLocations) <- c("deaths", "longitude","latitude","type" )
  # combine the two data files into one so it's easier for the program to parse it 
  joined <- rbind(deathLocations,pumpLocations)

# start up the gui 
ui <- dashboardPage(
  
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 1"),
  dashboardSidebar(disable = TRUE),
  
  #start of the body 
  dashboardBody(
    #give style for the map
    tags$style(type = "text/css", "#map {height: calc(100vh - 240px) !important;}"),
    tabsetPanel( 
      #first tab
      tabPanel("Deaths and attacks",
        fluidRow(
        box( title = "Deaths in 1854 from the London cholera outbreak", solidHeader = TRUE, status = "primary", width = 10,
             plotOutput("tab1barchart"))
      ),
      box(title = "Deaths and Attacks ", solidHeader = TRUE, status = "primary", width = 10,
          dataTableOutput("tab1table")
        )
      ),
      #second tab
      tabPanel("Age/Sex", 
               
      fluidRow(
        box(title = "Males vs Females fatalities (Thousands)", solidHeader = TRUE, status = "primary", width = 4,
            dataTableOutput("tab2table")),
        box( title = "Male fatalities (Thousands)", solidHeader = TRUE, status = "primary",width = 3,
             plotOutput("tab2histogram0")),
        box( title = "Female fatalities (Thousands)", solidHeader = TRUE, status = "primary",width = 3,
             plotOutput("tab2histogram1"))
      )),
      tabPanel("Census data",
               fluidRow(
                 box(title = "Census data (United Kingdom 1851) ", solidHeader = TRUE, status = "primary",width = 3,
                     dataTableOutput("tab3table")),
                 box( title = "Males", solidHeader = TRUE, status = "primary", width = 4,
                      plotOutput("censusBar0")),
                 box( title = "Females", solidHeader = TRUE, status = "primary",width = 4,
                      plotOutput("censusBar1")),
                 box( title = "Males", solidHeader = TRUE, status = "primary", width = 3,
                      plotOutput("piechart0")),
                 box( title = "Females", solidHeader = TRUE, status = "primary",width = 3,
                      plotOutput("piechart1")),
                 box( title = "Males vs Females", solidHeader = TRUE, status = "primary", width = 3,
                      plotOutput("piechart2"))
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
  output$tab1table <- DT::renderDataTable(
    DT::datatable({ 
      # copy the data to ensure it doesn't change 
      deathsTable <-  deaths
    }, 
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
     ) 
    )
  )

  
  output$tab2table <- DT::renderDataTable(
    DT::datatable({ 
      ageAndSexTable <-  ageAndSex
      }, 
    class = 'cell-border stripe',
    rownames = FALSE,
    options = list(searching = FALSE, pageLength = 9, lengthChange = FALSE,dom = 't', order = list(list(0, 'asc'))
                   
    ) 
    )
  )
  
  output$tab3table <- DT::renderDataTable(
    DT::datatable(census, 
                  options = list(searching = FALSE,dom = 't',pageLength = 9, lengthChange = FALSE,
                                 columnDefs = list(list(className = 'dt-left', 
                                                        targets = 0:4)))) %>% 
      formatCurrency(2:4, ''),class = 'cell-border stripe'
  )
  
  
#graph outputs 
  output$tab1barchart <- renderPlot({
    #combine the data by date 
    my_data <- melt(deaths, id ="Date")
    #plot the bar chart, with each col's color being dependent on the variable 
    ggplot(my_data, aes(Date, value,colour = variable,group=variable)) +
      geom_line() +   
      geom_point(size = 1) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 0)) 
 })
  
  # output the two histograms/bar graphs for the second tab. 
  output$tab2histogram0 <- renderPlot({
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
  
  output$tab2histogram1 <- renderPlot({
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
  
  # create the different pie charts for the census data 
  output$piechart0 <- renderPlot({
    df <- data.frame(
      group = census$age,
      # calculate value as the percentage sum 
      value = round(census$male/sum(census$male)*100)
    )
    
    ggplot(df, aes(x ="",value,fill = group)) +
      geom_bar(width = 4, stat="identity")  +
      geom_text(aes(x = 3.5,label = percent(value/100)), size=3.5, position = position_stack(vjust = 0.5)) +
      coord_polar("y") +
      scale_fill_brewer(palette="Dark2") +
      theme(axis.text.x =element_blank()) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  
  output$piechart1 <- renderPlot({
    
    df <- data.frame(
      group = census$age,
      value = round(census$female/sum(census$female)*100)
    )
    
    ggplot(df, aes(x ="",value,fill = group)) +
      geom_bar(width = 4, stat="identity")  +
      geom_text(aes(x = 3.5,label = percent(value/100)), size=3.5, position = position_stack(vjust = 0.5)) +
      coord_polar("y") +
      scale_fill_brewer(palette="Dark2") +
      theme(axis.text.x =element_blank()) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  output$piechart2 <- renderPlot({

    totalMale <- sum(census$male)
    totalFemale <- sum(census$female)
    
    myDF <- data.frame(
      group = c("Male", "Female"),
      value = c(totalMale, totalFemale)
    )
    
    ggplot(myDF, aes(x ="",value,fill = group)) +
      geom_bar(width = 1, stat="identity")  +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Dark2") +
      geom_text(aes(label = percent((value/(totalMale+totalFemale)))), size=10, position = position_stack(vjust = 0.5)) +
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
    #red is for the deaths, while blue is for the wells 
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
    #set starting position to one of the locations from the data file 
    m <- setView(m, lng = -0.136668,lat = 51.513341 , zoom = 16)
    m
  })
  
}

#start the actual application 
shinyApp(ui = ui, server = server)