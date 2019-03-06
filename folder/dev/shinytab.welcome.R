library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Subreddit hyperlinks"),  #Limited length. Main title.
  dashboardSidebar(
    sidebarMenu( #This is where you define the left menu objects and their targets. 
      #Note: these need to match the tabItem (tabNames) 
      menuItem("Intro",  tabName = "Welcome",   icon = icon("dashboard")),
      menuItem("Events", tabName = "Network",   icon = icon("th"))
    ) #sidebarMenu end
  ), #dashboardSidebar end
  dashboardBody(
    #The top element is a tab, contained in the tabItems object
    tabItems(
      
      tabItem(tabName = "Network",           
              fluidRow(                                # Boxes need to be put in a row (or column)
                box(strong("Orlando Shooting"),
                    p("This event concerns the shooting that took place in Orlando and killed 49 people"),
                    actionButton("OS", "Enter")
                ),
                
                box(strong("Ferguson unrest"),
                    p("The fatal shooting of Michael Brown caused a lot of protests and riots in Ferguson, Missouri"),
                    actionButton("OS", "Enter")
                ),
                
                box(strong("Orlando Shooting"),
                    p("This event concerns the shooting that took place in Orlando and killed 49 people"),
                    actionButton("OS", "Enter")
                ),
                
                box(strong("Orlando Shooting"),
                    p("This event concerns the shooting that took place in Orlando and killed 49 people"),
                    actionButton("OS", "Enter")
                )
                
              ) #fluidRow end
      ) #tab end
    ) #tabItems end
  ) #dashboard body end
) #dashboardpage end

#SERVER
server <- function(input, output) {
  set.seed(122)          #unnecessary
  histdata <- rnorm(500) # define data for plot
  
  output$plot1 <- renderPlot({              #define plot
    data       <- histdata[seq_len(input$slider)]
    hist(data)                               #call plot with data
  }) #end of plot block
} #end of server block

shinyApp(ui, server)
