#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)
library(plotly)
#load("orlando_new.Rmd")

ui <- dashboardPage(
  
  dashboardHeader(title = h3("Subreddit hyperlinks")),  #Limited length. Main title.
    dashboardSidebar(
     sidebarMenu( #This is where you define the left menu objects and their targets. 
              #Note: these need to match the tabItem (tabNames) 
             menuItem("Intro",  tabName = "Welcome",   icon = icon("dashboard")),
              menuItem("Events", tabName = "Network",   icon = icon("th"))
            ) #sidebarMenu end
          ), #dashboardSidebar end
          
    dashboardBody(
  fluidPage(
  titlePanel("Orlando shooting network graph"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Feel people's emotions"),
      
      selectInput("var", 
                  label = "Choose an option",
                  choices = c("1. Which subreddits have the most number of posts", "2. Which subreddits observe a change in sentiment",
                              "3. Which subreddits will see a probably post"),
                  selected = "1. Which subreddits have the most number of posts"),
      
      sliderInput("range", 
                  label = "Select the event surrounding dates",
                  min = 5, max = 15, value = c(10), step=5)
      ),
    
      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotOutput("orlando_network1"), plotOutput("orlando_network2"))
        )
      )
                           # dblclick = "plot_dblclick",
                           # brush = brushOpts(
                           #   id = "plot_brush",
                           #   resetOnNew = TRUE))
    )        
  )
 )
 
)

server <- function(input, output) {
  
  # ranges <- reactiveValues(x = 10, y = 10)
  
  output$orlando_network1 <- renderPlot({
    plot(g2, layout = layout.fruchterman.reingold, 
         main = g2$name,
         vertex.color = "blue", 
         vertex.size = 4, 
         vertex.label = " ", 
         vertex.label.size = 0.1,
         edge.arrow.size = 0.2,
         edge.width=E(g2)$weight, 
         edge.color=ifelse(E(g2)$weight >= 0.00, "green", "red"))
    
  })

      output$orlando_network2 <- renderPlot({
    plot(g3, layout = layout.fruchterman.reingold, 
         main = g3$name,
         vertex.color = "blue", 
         vertex.size = 4, 
         vertex.label = " ", 
         vertex.label.size = 0.1,
         edge.arrow.size = 0.2,
         edge.width=E(g3)$weight, 
         edge.color=ifelse(E(g3)$weight >= 0.00, "green", "red"))
    
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  # observeEvent(input$plot1_dblclick, {
  #   brush <- input$plot1_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(brush$xmin, brush$xmax)
  #     ranges$y <- c(brush$ymin, brush$ymax)
  #     
  #   } else {
  #     ranges$x <- 10
  #     ranges$y <- 10
  #   }
  # 
  # 
  #    })
    
}  
    
    
# ui <- dashboardPage(
#   dashboardHeader(title = "Subreddit hyperlinks"),  #Limited length. Main title.
#   dashboardSidebar(
#     sidebarMenu( #This is where you define the left menu objects and their targets. 
#       #Note: these need to match the tabItem (tabNames) 
#       menuItem("Intro",  tabName = "Welcome",   icon = icon("dashboard")),
#       menuItem("Events", tabName = "Network",   icon = icon("th"))
#     ) #sidebarMenu end
#   ), #dashboardSidebar end
#   dashboardBody(
#     #The top element is a tab, contained in the tabItems object
#     
#     i <- fluidPage(
#       
#      # sidebarLayout(
#        # sidebarPanel(
#           selectInput("graph", 
#                       label = "Choose graph to display",
#                       choices = c("Simple plot", "Network Graph"), 
#                       selected = "Network Graph")), 
#         mainPanel(
#           plotOutput("myplot")
#         )
#       )
#       
#     )
#     

#           
#           
#     

shinyApp(ui, server)
