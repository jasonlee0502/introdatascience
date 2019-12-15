library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(spData)
library(dplyr)
library(plotly)
library(lubridate)

##Data - Run once everytime the app is open##
mapsg <- readRDS(file = "mapsg.rds")

#For Map Filters and Colour Coding
region <- c("North Region","Central Region","East Region","West Region","North-East Region")
room_type <- c("Private room","Entire home/Apartment","Shared room")
groupColors <- colorFactor(c("#E03A3C", "#009DDC","#62BB47"), room_type)

avg_price <- readRDS(file = "avg_price.rds")
fulldata <- readRDS(file = "fullairbnb.rds")

# Define server logic
shinyServer(function(input, output, session){
  
  #Overview Page ValueBoxes
  output$GoodReview <- renderValueBox({
    valueBox(
      "85%", "Good Reviews", icon = icon("thumbs-up", lib = "glyphicon"), color = "green"
    )
  })
  
  output$BadReview <- renderValueBox({
    valueBox(
      "20%", "Bad Reviews", icon = icon("thumbs-down", lib = "glyphicon"), color = "red"
    )
  })
  
  output$total_hosting <- renderValueBox({
    valueBox(
      value = length(unique(fulldata$'Host ID')), subtitle = "Number of Hosts",
      icon = icon("user", lib = "glyphicon"), color = "blue"
    )
  })
  
  output$total_listing <- renderValueBox({
    valueBox(
      value = nrow(fulldata), subtitle = "Number of Listings",
      icon = icon("home", lib = "glyphicon"), color = "teal"
    )
  })
  
  output$total_review <- renderValueBox({
    valueBox(
      value = sum(fulldata$'Number of Reviews'), subtitle = "Reviews Received",
      icon = icon("comment", lib = "glyphicon"), color = "maroon"
    )
  })
  
  output$total_price <- renderValueBox({
    valueBox(
      value = paste("$", round(sum(fulldata$Price)/1000,2)), subtitle = "Millions",
      icon = icon("usd", lib = "glyphicon"), color = "purple"
    )
  })
  
  output$plot_region <- renderPlotly({
    
    region_count <- fulldata %>%
                      group_by(Region) %>%
                      summarise(., n=n())
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    plot_ly(region_count, labels = ~Region, values = ~n, type = 'pie', hole = 0.9,
            textinfo = 'none' ,
            hoverinfo = 'text',text = ~paste('Total Listings:', n),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.25, y = 0.5)) %>%
      config(displayModeBar = F)
  })
  
  output$plot_roomType <- renderPlotly({
    
    roomType_count <- mapsg %>%
                        group_by(room_type) %>%
                        summarise(n=n())
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    plot_ly(roomType_count, labels = ~room_type, values = ~n, type = 'pie', hole = 0.9,
            textinfo = 'none' ,
            hoverinfo = 'text',text = ~paste('Total Listings:', n),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.22, y = 0.5)) %>%
      config(displayModeBar = F)
  })
  
  output$neighbourhoodList <- renderDataTable({
    NBHlist <- fulldata %>%
                  group_by(Neighbourhood) %>%
                  summarise(n=n()) %>% 
                  arrange(desc(n))
    colnames(NBHlist) <- c("Neighbourhood", "Number of Listings")
                  
    dat1 <- datatable(NBHlist[1:10,], options = list( initComplete = JS("function(settings, json) {
                                                                            $(this.api().table().header()).css({
                                                                            'background-color': '#fff',
                                                                            'color': '#444'
                                                                            });}")
            ,pageLength = 7, pagingType = "simple", bFilter = FALSE,bInfo = FALSE, bPaginate = FALSE), rownames= FALSE)
    return(dat1)
  })
  
  ##Singapore Map Visual##
  # reactivate map info
  mapdf <- reactive({
    mapsg %>%
      filter(neighbourhood_group %in% input$select_region & 
               room_type %in% input$select_room &
               price >= input$slider_price[1] &
               price <= input$slider_price[2] &
               no_of_review >= input$slider_review[1] &
               no_of_review <= input$slider_review[2]) 
  })
  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend(position = "bottomleft", pal = colorFactor(c("#E03A3C", "#009DDC","#62BB47"), room_type)
                , values = room_type, opacity = 1, title = "Room Type") %>% 
      setView(lng = 103.8198, lat = 1.3521, zoom = 13)
  })
  
  # observe an event
  observe({ #Require a trigger to call the observe function
    proxy <- leafletProxy("map",data = mapdf()) %>%
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      # circle
      addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, color = ~groupColors(room_type),
                       group = "CIRCLE",
                       popup  = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                       'Host:', host_name, '<br/>',
                                       'Neigbourhood:', neighbourhood, '<br/>',
                                       'Room Type:', room_type,'<br/>',
                                       'Price:', price,'<br/>',
                                       'Number of Reviews:', no_of_review,'<br/>')) %>% 
      # cluster
      addCircleMarkers(lng = ~long, lat = ~lat, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup  = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                       'Host:', host_name, '<br/>',
                                       'Neigbourhood:', neighbourhood, '<br/>',
                                       'Room Type: ', room_type, '<br/>',
                                       'Price:', price,'<br/>',
                                       'Number of Reviews:', no_of_review,'<br/>')) %>% 
      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  ## reactivate count dataframe for map graph1 
  countdf <- reactive({
    mapdf() %>%
      group_by(., room_type) %>%
      summarise(., count_type = n())
  })
  
  #map graph1 
  output$count_room <- renderPlotly({
    plot_ly(data = countdf(), x = ~room_type, y = ~count_type, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo = 'text',
            text = ~count_type) %>%
      layout(xaxis = list(title = "", showticklabels = FALSE), yaxis = list(title = "count"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~count_type, text = ~paste(round(count_type/sum(count_type),2)*100,'%'),
                                xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  ## reactivate price dataframe for map graph2
  pricedf <- reactive({
    mapdf() %>% 
      group_by(., room_type) %>% 
      summarise(., avg_price = round(mean(price),2))
  })
  
  #map graph2 avgprice
  output$avgprice <- renderPlotly({
    plot_ly(data = pricedf(), x = ~room_type, y = ~avg_price, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo = 'text',
            text = ~avg_price) %>% 
      layout(xaxis = list(title = "", showticklabels = FALSE), yaxis = list(title = "price"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~avg_price, text = ~paste('$',avg_price),
                                xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  ##### Listings, Region and Price Changes #######################
  ## reactivate dataframe for listings grapgh
  graph1df <- reactive({
    mapsg %>%
      filter(neighbourhood_group %in% input$tab2_region &
              price >= input$tab2_price[1] &
              price <= input$tab2_price[2] &
              no_of_review >= input$tab2_review[1] &
              no_of_review <= input$tab2_review[2]) %>% 
      group_by(neighbourhood_group, room_type) %>% 
      summarise(n=n())
  })
  
  # listings grapgh
  output$graph1 <- renderPlotly({
    t <- list(size = 9)
    plot_ly(data = graph1df(), x = ~n, y = ~room_type, type = "bar", color = ~neighbourhood_group,
            colors = c('#800080','#009DDC','#E03A3C','#62BB47','#FFA500'),
            orientation = 'h', showlegend = TRUE) %>%
      layout(xaxis = list(title = "count"), yaxis = list(title = ""), barmode = 'dodge', font = t)
  })
  
  # price change graph (year/ month)
  output$tab_price <- renderPlotly({
    if(input$price_option == 'Year'){
      
      price_Year <- avg_price %>%
        group_by(YearMonth) %>%
        summarise(total=sum(adjusted_price))
      
      m <- list(size = 8)
      plot_ly(data = price_Year, x = ~YearMonth, y = ~total, type = 'scatter', mode ='markers', linetype = I('solid')) %>% 
        layout(xaxis = list(title = "", showticklabels = TRUE), yaxis = list(title = "price"), showlegend = FALSE, font=m)
    } 
    else{
      price_Month <- avg_price %>%
        group_by(Month) %>%
        summarise(total=sum(adjusted_price))
      
      plot_ly(data = price_Month, x = ~Month, y = ~total, type= 'scatter', mode = 'markers+lines', color = "Set1",
        text = ~paste('Price: $', total)) %>% layout(xaxis = list(title = "month", type = 'category'), yaxis = list(title = "price"))
    }
  })
  
  #Output full list of data
  output$completeData <- renderDataTable(datatable({
    if(input$full_region != "All"){
      fulldata <- fulldata[fulldata$Region == input$full_region,]
    }
    
    if(input$full_neighbourhood != "All"){
      fulldata <- fulldata[fulldata$Neighbourhood == input$full_neighbourhood,]
    }
    if(input$full_roomType != "All"){
      fulldata <- fulldata[fulldata$'Room Type' == input$full_roomType,]
    }
    fulldata
      
  }))
  
})