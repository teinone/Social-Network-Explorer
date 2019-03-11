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
      theme_blue_gradient,
      tabItems(
            tabItem(tabName = "welcome",                     
              fluidRow(
                box(
                  title = "Reddit", width = "100%",
                  fluidRow(column(width = 10, p( "Paste introduction to Reddit and write our objectives here" )),
                           column(width = 2, align = "right",
                                  img(src="http://smirknewmedia.flywheelsites.com/wp-content/uploads/2018/03/snoo.jpg", width=100)))
                  ) #box end
              ), #fluidRow end
              fluidRow(
                box(
                  title = "Dataset description", width = "100%",
                  p("Please write here detailed description of the dataset")
                )#end of box
                
              ),#end of fluidRow
              
              fluidRow(                                # Boxes need to be put in a row (or column)
                box(width='100%',
                    strong("Orlando Shooting"),
                    p("This event concerns the shooting that took place in Pulse, a day nightclub in Orlando, killing 49 people and wounding 53 others. This is one of the deadliest incident of LGBT people in U.S. history. Scary, huh? Click on Enter and get to know the sentiment of thousands of people in the reddit forum."),
                    p("insert event picture below"),
                    img(src="http://smirknewmedia.flywheelsites.com/wp-content/uploads/2018/03/snoo.jpg", width=400, align = "center")
                )
              ),#fluidRow end
              
              fluidRow( 
                box(width='100%',
                    strong("Ferguson unrest"),
                    p("The fatal shooting of Michael Brown caused a lot of protests and riots in Ferguson, Missouri. The unrest resulted in a huge debate about the law reinforcement in the United States")
                )
              ),#fluidRow end
              
              fluidRow( 
                box(width='100%',
                    strong("Obama's visit to Cuba"),
                    p("Obama's visit to Cuba marks the first visit of an American president since 1928. It marks a huge change in relationship between U.S. and Cuba.")
                )
              ),#fluidRow end
              
              fluidRow(
                box(width='100%',
                    strong("Orlando Shooting"),
                    p("This event concerns the shooting that took place in Orlando and killed 49 people")
                )#fluidRow end
                
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
                              tabPanel("Sentiment analysis III", plotOutput("plotName4"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
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
                              tabPanel("Sentiment analysis III", plotOutput("plotName10"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
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
                              tabPanel("Sentiment analysis III", plotOutput("plotName16"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
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
                              tabPanel("Sentiment analysis III", plotOutput("plotName22"),
                                       p("Please  input summary here, and insert graph above."), 
                                       p("Remember to set graph width = '100%'")
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
