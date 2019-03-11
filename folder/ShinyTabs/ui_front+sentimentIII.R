# Load the necessary libraries
library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)     
library(ggplot2)        
library(stringr)
library(dygraphs)
library(plyr)
library(scales)
library(plotly)
library(shinyWidgets)
library(dashboardthemes)
library(igraph)
# Change working directory
getwd()
setwd("C:/Users/minxi/Documents/RFiles/Subreddit Network")

# ---------------------------------------------------------------------------------
# IMPORTANT
# 1. Please use plotly to render and call the graphs
# 2. Please keep all the functions in a *separate file* (instead of in the server)
# 3. Please make clear comments and follow name conventions
# 4. Please comment at the end of long statements e.g. "...), # end of tabItem"
# 5. DO NOT change the template for now until the App is integrated
# 6. Use the subsets Tiffany uploaded in Drive 'R Files and Data/Master Data'
# ---------------------------------------------------------------------------------

# Change dashboard design
statusBoxes <- lapply(shinydashboard:::validStatuses, function(status) {
  box(title = status, status = status, solidHeader = TRUE, width = 2)
})

colorBoxes <- lapply(shinydashboard:::validColors, function(color) {
  box(title = color, background = color, width = 2)
})

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    
    dashboardHeader(title = "Subreddit Network Analysis"),  #Limited length. Main title.
    dashboardSidebar( 
      sidebarMenu( #This is where you define the left menu objects and their targets. 
        #Note: these need to match the tabItem (tabNames) 
        menuItem("Introduction",  tabName = "welcome",   icon = icon("th")),
        menuItem("Event analysis", tabName = "events",   icon = icon("th"),
          menuSubItem("Orlando Shooting",  tabName = "orlando"),
          menuSubItem("Ferguson Unrest",  tabName = "ferguson"),
          menuSubItem("Obama's Visit to Cuba",  tabName = "obama"),
          menuSubItem("Trump",  tabName = "trump")
                 )# end of menuItem
      ) #sidebarMenu end
    ), #dashboardSidebar end
    dashboardBody(
      theme_grey_dark,
      tabItems(
            tabItem(tabName = "welcome",                     
              fluidRow(
                box(
                  title = "Reddit", width = "100%",
                  fluidRow(
                    column(width = 2, align = "center",
                           img(src="https://images-eu.ssl-images-amazon.com/images/I/418PuxYS63L.png", width=100)),
                    column(width = 10,  htmlOutput("description_reddit"))
                           )
                  ) #box end
              ), #fluidRow end
              fluidRow(
                box(
                  title = "Dataset description", width = "100%",
                  strong('Subreddit Hyperlink Network:'),
                  htmlOutput("description_dataset1"),
                  img(src = "https://i.ibb.co/dgtyqXc/stats-reddit.png"),
                  htmlOutput("description_dataset2")
                )#end of box
              ),#end of fluidRow
              
              fluidRow(                                # Boxes need to be put in a row (or column)
                box(width='100%',
                    title = "Orlando Shooting",
                    htmlOutput("description_orlando"),
                    img(src="https://media.myfoxmemphiscom.cmgdigital.com/photo/2016/06/12/Orlando%20MASS%20SHOOTING_SOCIAL%20POST_1465760478177_4779522_ver1.0_1280_720.png", width = "100%", align = "center")
                )
              ),#fluidRow end
              
              fluidRow( 
                box(width='100%',
                    title = "Ferguson unrest",
                    htmlOutput("description_ferguson"),
                    img(src="https://cdn5.img.sputniknews.com/images/104747/48/1047474896.jpg", width="100%", align = "center")                )
              ),#fluidRow end
              
              fluidRow( 
                box(width='100%',
                    title = "Obama's visit to Cuba",
                    htmlOutput("description_obama"),
                    img(src="https://cdn.cnn.com/cnnnext/dam/assets/160321121508-07-obama-castro-0321-super-169.jpg", width="100%", align = "center")                )
              ),#fluidRow end
              
              fluidRow(
                box(width='100%',
                    title = "Trump as Time's Person of the Year",
                    htmlOutput("description_trump"),
                    img(src="https://www.washingtonpost.com/rf/image_1484w/2010-2019/Wires/Images/2016-12-07/Reuters/2016-12-07T132556Z_01_TOR500R_RTRIDSP_3_USA-TRUMP-TIME.jpg?t=20170517", width="100%", align = "center")                
                    )#box end
              ) #fluidRow end
              
            ), #tabItem end
      tabItem(tabName = "orlando",                     
              fluidRow(
                box(
                  title = "Event Overview: Orlando Shooting", width = "100%",
                  tabsetPanel(type = "tabs", 
                              # Example
                              tabPanel("Example", 
                                       # Application title
                                       titlePanel("Old Faithful Geyser Data"),
                                       # Sidebar with a slider input for number of bins 
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("bins",
                                                       "Number of bins:",
                                                       min = 1,
                                                       max = 50,
                                                       value = 30)
                                         ),
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput("distPlot")
                                         )
                                       )
                                       
                              ),# end of tabPanel
                              # Histogram
                              tabPanel("No. of posts over time", plotOutput("plotName1"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                                       ),# end of tabPanel
                              # Tiffany's compound graph
                              tabPanel("Sentiment analysis I", verbatimTextOutput("summary2"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                                       ),# end of tabPanel
                              # Lizzie's sentiment distribution graph
                              tabPanel("Sentiment analysis II", tableOutput("table3"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                                       ),# end of tabPanel
                              # Minxin's graph
                              tabPanel("Sentiment analysis III", 
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("select.subreddit.orlando", "Highlight subreddit:",
                                                       choices = c("the_donald", "pics", "iama", "todayilearned", "funny", "videos", "worldnews"),
                                                       selected = NA),
                                           verbatimTextOutput("value.outbound.orlando"),
                                           width = 400
                                         ),
                                         mainPanel(
                                           plotlyOutput("plot.scatter.sentiment.orlando", height = 550, width = "100%"), 
                                           height = 550, width = "100%"
                                           
                                         ) 
                                       )#siderbar end
                                       
                                       
                                       ),#end of tabPanel
                              # Subreddit network
                              tabPanel("Subreddit network", plotOutput("plotName5"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                                       ),#end of tabPanel
                              # Degree distribution
                              tabPanel("Degree distribution", plotOutput("plotName6"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                                       )#end of tabPanel
                              
                  )#end of tabsetPanel
                  
                ) #box end
              ) #fluidRow end
      ), #tabItem end
      tabItem(tabName = "ferguson",                     
              fluidRow(
                box(
                  title = "Event Overview: Ferguson Unrest", width = "100%",
                  tabsetPanel(type = "tabs", 
                              # Histogram
                              tabPanel("No. of posts over time", plotOutput("plotName7"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Tiffany's compound graph
                              tabPanel("Sentiment analysis I", verbatimTextOutput("summary8"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Lizzie's sentiment distribution graph
                              tabPanel("Sentiment analysis II", tableOutput("table9"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Minxin's graph
                              tabPanel("Sentiment analysis III", 
                                       sidebarLayout(
                                sidebarPanel(
                                  selectInput("select.subreddit.ferguson", "Highlight subreddit:",
                                              choices = c("askreddit", "iama", "pics", "funny", "videos", "worldnews", "todayilearned"),
                                              selected = NA),
                                  verbatimTextOutput("value.outbound.ferguson"),
                                  width = 400
                                ),
                                mainPanel(
                                  plotlyOutput("plot.scatter.sentiment.ferguson", height = 550, width = "100%"), 
                                  height = 550, width = "100%"
                                ) 
                              )#siderbar end
                              ),#end of tabPanel
                              # Subreddit network
                              tabPanel("Subreddit network", plotOutput("plotName11"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),#end of tabPanel
                              # Degree distribution
                              tabPanel("Degree distribution", plotOutput("plotName12"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              )#end of tabPanel
                  )#end of tabsetPanel
                ) #box end
              ) #fluidRow end
      ), #tabItem end
      tabItem(tabName = "obama",                     
              fluidRow(
                box(
                  title = "Event Overview: Obama's Visit to Cuba", width = "100%",
                  tabsetPanel(type = "tabs", 
                              # Histogram
                              tabPanel("No. of posts over time", plotOutput("plotName13"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Tiffany's compound graph
                              tabPanel("Sentiment analysis I", verbatimTextOutput("summary14"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Lizzie's sentiment distribution graph
                              tabPanel("Sentiment analysis II", tableOutput("table15"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Minxin's graph
                              tabPanel("Sentiment analysis III", 
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("select.subreddit.obama", "Highlight subreddit:",
                                                       choices = c("pics", "todayilearned", "sandersforpresident", "worldnews", "iama", "politics", "funny"),
                                                       selected = NA),
                                           verbatimTextOutput("value.outbound.obama"),
                                           width = 400
                                         ),
                                         mainPanel(
                                           plotlyOutput("plot.scatter.sentiment.obama", height = 550, width = "100%"), 
                                           height = 550, width = "100%"
                                         ) 
                                       )#siderbar end
                              ),#end of tabPanel
                              # Subreddit network
                              tabPanel("Subreddit network", plotOutput("plotName17"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),#end of tabPanel
                              # Degree distribution
                              tabPanel("Degree distribution", plotOutput("plotName18"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              )#end of tabPanel
                  )#end of tabsetPanel
                ) #box end
              ) #fluidRow end
      ), #tabItem end
      tabItem(tabName = "trump",                     
              fluidRow(
                box(
                  title = "Event Overview: Trump as Time's Person of the Year", width = "100%",
                  tabsetPanel(type = "tabs", 
                              # Histogram
                              tabPanel("No. of posts over time", plotOutput("plotName19"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Tiffany's compound graph
                              tabPanel("Sentiment analysis I", verbatimTextOutput("summary20"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Lizzie's sentiment distribution graph
                              tabPanel("Sentiment analysis II", tableOutput("table21"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),# end of tabPanel
                              # Minxin's graph
                              tabPanel("Sentiment analysis III", 
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("select.subreddit.trump", "Highlight subreddit:",
                                                       choices = c("the_donald", "pics", "videos", "worldnews", "iama", "news", "aww"),
                                                       selected = NA),
                                           verbatimTextOutput("value.outbound.trump"),
                                           width = 400
                                         ),
                                         mainPanel(
                                           plotlyOutput("plot.scatter.sentiment.trump", height = 550, width = "100%"), 
                                           height = 550, width = "100%"
                                         ) 
                                       )#siderbar end
                              ),#end of tabPanel
                              # Subreddit network
                              tabPanel("Subreddit network", plotOutput("plotName23"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              ),#end of tabPanel
                              # Degree distribution
                              tabPanel("Degree distribution", plotOutput("plotName24"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
                              )#end of tabPanel
                              
                  )#end of tabsetPanel
                ) #box end
              ) #fluidRow end
      ) #tabItem end
      
            ) #tabItems end
          )#dashboard Body end
        ) #dashboard Page end
      ) #shinyUI end
