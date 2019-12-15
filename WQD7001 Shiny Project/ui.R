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

fulldata <- readRDS(file = "fullairbnb.rds")

#User Interface of Shiny App
dashboardPage(title = "WQD7001 Group Project",
              skin = ("purple"),
              dashboardHeader(
                title = "Singapore Airbnb",
                              
                # Email Sharing link
                tags$li(class = "dropdown",
                        tags$a(tags$img(height = "18px", 
                                        src = "images/email.png")
                        )
                ),
                
                # Twitter Sharing Link
                tags$li(class = "dropdown",
                        tags$a(tags$img(height = "18px", 
                                        src = "images/twitter.png")
                        )
                ),
                
                # Facebook Sharing link
                tags$li(class = "dropdown",
                        tags$a(tags$img(height = "18px", 
                                        src = "images/facebook.png")
                        )
                ),
                
                # LinkedIn Sharing link
                tags$li(class = "dropdown",
                        tags$a(tags$img(height = "18px", 
                                        src = "images/linkedin.png")
                        )
                )
                              
              ),
              
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Overview", tabName = "overview", icon = icon("home")),
                  menuItem("Singapore Map View", tabName = "mapview", icon = icon("map")),
                  menuItem("Analysis", tabName = "analysis", icon = icon("binoculars")),
                  menuItem("Data", tabName = "datafile", icon = icon("file-pdf-o")),
                  menuItem("About", tabName = "refer", icon = icon("info")),
                  hr(),
                  h4("\t\tGroup Members:"), 
                  sidebarUserPanel(name = "Jason Lee", 
                                   subtitle = "Finance Analyst"),
                  sidebarUserPanel(name = "Baqir Hashmani", 
                                   subtitle = "Business Analyst"),
                  
                  hr(),
                  menuItem("Source code", icon = icon("file-code-o"), 
                           href = "https://github.com/jasonlee0502/introdatascience/tree/master/WQD7001%20Shiny%20Project"),
                  menuItem("Bug Reports", icon = icon("bug"),
                           href = "https://github.com/jasonlee0502/introdatascience/issues")
                )
              ),
              
              dashboardBody(
                
                tabItems(
                  # Overview Tab
                  tabItem(tabName = "overview",
                          column(12, h2("Singapore AirBnb Analysis Dashboard")),
                          br(),
                          br(),
                          
                          fluidRow(
                            infoBox(h4("Number of users visit today:"), sample(200:1000, 1)),
                            valueBoxOutput("GoodReview"),
                            valueBoxOutput("BadReview")
                          ),
                          
                          box(
                            title = "Overall",
                            status = "primary",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            
                            fluidRow(
                              box(
                                status = "primary",
                                width = 12,
                                solidHeader = FALSE,
                                collapsible = TRUE,
                                valueBoxOutput("total_hosting",width = 3),
                                valueBoxOutput("total_listing", width = 3),
                                valueBoxOutput("total_review", width = 3),
                                valueBoxOutput("total_price", width = 3)
                              )
                            ),
                            
                            fluidRow(
                              column(12, align = "center",
                                     box(
                                       title = "Listings By Regions",
                                       status = "primary",
                                       width = 3,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot_region", height = 250)
                                     ),
                                     
                                     box(
                                       title = "Listings by Room Type",
                                       status = "primary",
                                       width = 3,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot_roomType", height = 250)
                                     ),
                                     
                                     box(
                                       title = "Top 10 Number of Listings by Singapore Neighbourhood",
                                       status = "primary",
                                       width = 6,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       style = "color: #444",
                                       dataTableOutput("neighbourhoodList")
                                     )
                              )
                            )
                          )
                  ),
                  
                  # Singapore Map View Tab
                  tabItem(tabName = "mapview",
                          fluidRow(
                                    column(8, align = "right",
                                    div(class="outer", tags$head(includeCSS("styles.css")),
                                    leafletOutput(outputId = "map", width = "88%", height = "100%"))
                            )
                          ),
                                
                          # Panel options: region, Room Type, Price, Reviews
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                                        top = 120, left = "auto", right = 20, bottom = "auto",
                                        width = 320, height = "auto",
                                        h2("Airbnb in Singapore"),
                                        checkboxGroupInput(inputId = "select_region", label = h4("Region"), 
                                                           choices = region, selected = 'Central Region'),
                                        checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                           choices = room_type, selected = room_type),
                                        sliderInput(inputId = "slider_price", label = h4("Price"), min = 1, max = 1000, step = 50,
                                                    pre = "$", sep = ",", value = c(65, 8900)),
                                        sliderInput(inputId = "slider_review", label = h4("Number of Reviews"), min = 5, max = 250, step = 50,
                                                    value = c(10, 450)),
                                        h6("The map information is updated as at 25 Sept, 2019 retrieved from"),
                                        h6(a("Inside Airbnb", href = "http://insideairbnb.com/get-the-data.html", target="_blank"))
                          ),
                          
                          # Results: count_room, avgprice
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                                        top = 600, left = 350, right = "auto" , bottom = "auto",
                                        width = 320, height = "auto",
                                        plotlyOutput(outputId = "count_room",height = 150),
                                        plotlyOutput(outputId = "avgprice", height = 150))
                  ),
                  
                  # Analysis Tab
                  tabItem(tabName = "analysis",
                          tabPanel("Listings, Region and Price Changes",    
                                   fluidRow(
                                     column(3,
                                            h3("Room Type by Region and Room Type"),
                                            br(),
                                            br(),
                                            checkboxGroupInput(inputId = "tab2_region", label = h4("Region"), choices = region, selected = region),
                                            sliderInput(inputId = "tab2_price", h4("Price/Night"), min = 1, max = 1000, value = c(10, 890)),
                                            sliderInput(inputId = "tab2_review", h4("Number of Review"), min = 5, max = 250, value = c(40,450)),
                                            br(),
                                            h3("Price Changes over Time"),
                                            selectInput("price_option", label = h3("Select Type of View"), 
                                                        choices = list("Year" = "Year","Month" = "Month"), selected = "Year")
                                     ),
                                     
                                     column(9,
                                            h3(""),
                                            plotlyOutput(outputId = "graph1", width=1000, height =350),
                                            br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                            plotlyOutput(outputId = "tab_price", width=1000, height =350))
                                   )
                          )
                  ),
                  
                  # Airbnb Data Tab
                  tabItem(tabName = "datafile",
                          column(12, h2("Airbnb Complete Data")),
                          br(),
                          br(),
                          fluidRow(
                            column(4,
                                   selectInput("full_region",
                                               "Region:",
                                               c("All", region))
                            ),
                            column(4,
                                   selectInput("full_neighbourhood",
                                               "Neighbourhood:",
                                               c("All", unique(as.character(fulldata$Neighbourhood))))
                            ),
                            column(4,
                                   selectInput("full_roomType",
                                               "Room Type:",
                                               c("All", room_type))
                            )
                          ),
                          br(),
                          dataTableOutput("completeData")
                  ),
                  
                  
                  # Reference Tab
                  tabItem("refer",
                          ##### References ##########
                           h3("Data Source Obtained from: "),
                           tabPanel("Inside Airbnb",
                           h3("Inside Airbnb", a("Link", href="http://insideairbnb.com/get-the-data.html")))
                  )
                )
              )
)
