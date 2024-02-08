library(dplyr)
library(tidygeocoder)
library(readr)
library(janitor)
library(tidyverse) # separte function 
#mapping
library(leaflet)
#shiny
library(shiny)
library(shinythemes)
library(DT)

#functions 
## Function 1: find lat and long of df
geo_function <- function(df){
  df1 <- df%>% 
    mutate(country = 'Singapore') %>% 
    tidygeocoder:: geocode(
      street  = 'town',
      country=  'country',
      method = 'osm')
  #manually adding missing lat and long values
  df1$lat[df$town== 'CENTRAL AREA'] <- 1.290439
  df1$long[df$town== 'CENTRAL AREA'] <- 103.848571
  df1$lat[df$town== 'KALLANG/WHAMPOA'] <- 1.3100
  df1$long[df$town== 'KALLANG/WHAMPOA'] <- 103.8651
  return (df1)
}

## Function 2: allocate price range 
price_range_function <- function(df){
  df$average_resale_range <- cut (df$average_resale_price,
                                  breaks = c(quantile(df$average_resale_price,probs = c(0,0.25,0.5,0.75,1))),
                                  labels = c ("Budget", "Low", "Mid","High"),
                                  include.lowest =TRUE)
  return(df)
}


#import data 
hdb_resale_df <- read.csv("C:/Users/yeexu/Desktop/adulting/Data_science/Google Data Analytics/capstone/SG_housing_transaction/dataset2/ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")
## cleaning the data
hdb_resale_df$date <- hdb_resale_df$month
hdb_resale_df<- hdb_resale_df %>% 
  separate(month,into=c('year', 'month'),sep = '-')

#Pivot table: average price base on flat_type in particular region, over the years
p_df <- hdb_resale_df %>% 
  group_by(year,town,flat_type) %>% 
  summarize(average_resale_price= round(mean(resale_price),0), no_of_resale = n())

#Cleaning up street name 
hdb_resale_df_add<- hdb_resale_df 
hdb_resale_df_add$street_name<- hdb_resale_df_add$street_name%>%  
  {gsub('LOR','LORONG',.)} %>% 
  {gsub('NTH','NORTH',.)} %>% 
  {gsub("C'WEALTH",'COMMONWEALTH',.)} %>%
  {gsub(' CRES',' CRESCENT',.)} %>% 
  {gsub(' CL',' CLOSE',.)} %>% 
  {gsub('UPP','UPPER',.)} %>% 
  {gsub('BT','BUKIT',.)} %>% 
  {gsub('CTRL','CENTRAL',.)} %>% 
  {gsub('TG ','TANJONG ',.)} %>% 
  {gsub('YUNG PING RD','CORPORATION RD',.)}

hdb_resale_df_add$country<- "SINGAPORE"

#creating dataframe
geo_p_df<- geo_function (p_df)


#rShiny
ui <- fluidPage(
    tags$style("
    #controls {background-color: #ddd; opacity: 0.7;overflow: auto;}
    #controls:hover{ opacity: 97;} 
               "),
  theme = shinytheme("cerulean"),
  headerPanel("Singapore HDB Resale Flats 2017 - 2024"),
  sidebarPanel(width = 2,
    HTML("<h3> Input Parameters</h3>"),
    selectInput("year", label = "Year:",
              choices = c("2017","2018","2019","2020","2021","2022","2023","2024"),
              selected = "2023"),
    selectInput("flat_type", label="Flat Type:", 
              choices = c("1 ROOM" = "1 ROOM","2 ROOM" = "2 ROOM",
                            "3 ROOM" = "3 ROOM", "4 ROOM" = "4 ROOM",
                            "5 ROOM" = "5 ROOM", "EXEC" = "EXECUTIVE",
                            "MULTI-GEN"="MULTI-GENERATION"),
              selected = "4 ROOM")),
  
  
  mainPanel(width = 6,
    leafletOutput("sgmap",height = 650,width=900)),
  
  absolutePanel(id = "controls",class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "90%", 
                h2('In-depth Explorer'),
                br(),
                sliderInput(inputId = "priceBins",
                            label = "Number of bins:",
                            min = 0,
                            max = 100,
                            value = 50),
                plotOutput("count_plot"),
                sliderInput(inputId = "areaBins",
                            label = "Number of bins:",
                            min = 0,
                            max = 100,
                            value = 30),
                plotOutput("area_plot"),
                plotOutput("model_plot")
  )
)


server <- function(input, output, session) {
  filteredData <- reactive({
    geo_p_df %>% 
      filter(year %in% input$year & flat_type %in% input$flat_type ) %>% 
      price_range_function()
  })
  
  output$sgmap <- renderLeaflet({
     # Defining color palette 
      pal = colorFactor(palette= c("green","yellow","orange", "red"), domain=filteredData()$average_resale_range)
     
      leaflet(data = filteredData()) %>% 
        addTiles() %>% 
        addCircles(lng = ~long, 
                   lat = ~lat,
                   label = ~town,
                   weight = 1,
                   radius = ~((filteredData()$average_resale_price)/1000)*1.5,
                   popup = ~paste(filteredData()$town, "<br>Year:",filteredData()$year,
                                  "<br>Flat Type:", filteredData()$flat_type,
                                  "<br>No of Resale:", filteredData()$no_of_resale,
                                  "<br>Average Resale Price:$", format(filteredData()$average_resale_price,big.mark = ",",scientific=FALSE)),
                   color = ~pal (filteredData()$average_resale_range),
                   fillOpacity = 0.75,
                   highlightOptions = highlightOptions(weight = 1, 
                                                       color = "#6897bb", 
                                                       fillColor = "#004b88"),
                   layerId = filteredData()$town,
                   popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>% 
        addLegend("bottomleft", pal = pal,
                  values = ~filteredData()$average_resale_range,
                  title = "Avg Resale Price",
                  opacity = 1)
  })
  
  ggplot_data <- reactive({
    town <- input$sgmap_shape_click$id 
    year <- input$year
    flat_type <- input$flat_type
    hdb_resale_df[hdb_resale_df$town %in% town & 
                    hdb_resale_df$year %in% year & 
                    hdb_resale_df$flat_type %in% flat_type,]})
  ## price 
  output$count_plot <- renderPlot(
    ggplot(data =ggplot_data(), aes(x =resale_price)) +
      geom_histogram(aes(y=..density..),bins =input$priceBins, color = '#018571',fill = '#80CDC1')+
      geom_density( alpha = 0.3, color = "#0571B0",fill="#92C5DE")+
      geom_vline(aes(xintercept=mean(resale_price)),
                 color="#404040", linetype="dashed", linewidth=1)+
      scale_x_continuous(labels = scales::comma)+
      ggtitle("Resale Price Range")+
      xlab("Resale Price")
  )
  
  ## area
  output$area_plot <- renderPlot(
    ggplot(data =ggplot_data(), aes(x = floor_area_sqm)) +
      geom_histogram(aes(y=..density..),bins = input$areaBins, color = '#018571',fill = '#80CDC1')+
      geom_density( alpha = 0.1, color = "#0571B0",fill="#92C5DE")+
      geom_vline(aes(xintercept=mean(floor_area_sqm)),
                 color="#404040", linetype="dashed", linewidth=1)+
      ggtitle("Resale Floor Area Range")+
      xlab("Floor Area (sqm)")
  )
  
  ## flat model
  output$model_plot <- renderPlot(
        ggplot(data =ggplot_data(), aes(x= fct_infreq(flat_model)))+
          geom_bar( color = '#018571',fill = '#80CDC1')+
          geom_text(stat = "count", aes(label = after_stat(count)), vjust=-0.5, size = 3)+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          ggtitle("Flat Model Frequency")+
          xlab("Flat Model") + ylab("Count")
      ) 

  }
shinyApp(ui, server)

