# Libraries:

#install.packages("plotly")
#install.packages("shinydashboard")


library(ggplot2)
library(igraph)
library(plotly)
library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)



# UI:
ui <- dashboardPage(
        dashboardHeader(title = "Subreddit hyperlinks"),  #Limited length. Main title.
        dashboardSidebar(
          sidebarMenu( #This is where you define the left menu objects and their targets. 
            #Note: these need to match the tabItem (tabNames) 
            menuItem("Welcome",  tabName = "Welcome",   icon = icon("dashboard")),
            menuItem("Network", tabName = "Network",   icon = icon("th"))
          ) #sidebarMenu end
        ), #dashboardSidebar end
  dashboardBody(
    #The top element is a tab, contained in the tabItems object
    tabItems(
      tabItem(tabName = "Welcome",                #This needs to match the tabName in the menu
        fluidRow(                                 # Boxes need to be put in a row (or column)
          
          #Tab 1: Box no 1: Histogram
          box(
            plotOutput("plot1", height = 250)), # you can call any output element here
          
          #Tab 1: Box no 2: Slider          
          box(
            title = "Controls",
            sliderInput("slider", "Number of cats involved:", 1, 100, 50) #min, max, default 
            ) #box end
          ) #fluidRow end
        ), #tab end
      
      tabItem(tabName = "Network",               # Here we define a new 
        fluidRow(                                # Boxes need to be put in a row (or column)
          #Box no 1:
          box(   
            plotlyOutput("plotly3d"),            # Plotly "plot"
            verbatimTextOutput("event")),        # basically print text from output$event, which in this case is point attributes
          
          box( 
            plotlyOutput("plotly1")
           # title = "Cat controls, doesn't do anything yet",
           #  sliderInput("slider", "Number of cats involved:", 1, 100, 50) #min, max, default 
          ) #box end
        ) #fluidRow end
      ) #tab end
    ) #tabItems end
  ) #dashboard body end
) #dashboardpage end

#SERVER
server <- function(input, output) {
  #DATA OPERATIONS:
  
  set.seed(122)          #unnecessary, random seed
  histdata <- rnorm(500) # define random data for plot

  # PLOTS:
  
  # PLOT1:  This is the base::plot element
  output$plot1 <- renderPlot({                    # define plot
    data       <- histdata[seq_len(input$slider)] # data to call, include the slider input in the data
    hist(data)                                    # call plot with data
  }) # end of output$plot1 block
  
  
  # This is the plotly element.   
  # renderPlotly() also understands ggplot2 objects!
  output$plotly3d <- renderPlotly({
      plot_ly(x = rnorm(10),            # data to use
              y = rnorm(10),
              z = rnorm(10),
              type = "scatter3d",
              mode = "markers")       # plot type 
  }) # end of output#plotly3d 
  
  output$plotly1 <- renderPlotly({
    plot_ly(hist(data)            # data to use
            #x = 50,
            ) 
  }) # end of output#plotly1 
  
  #This is the plotly hover text element. This shows data point info when the user hovers over it.
  output$event <- renderPrint({
      event1 <- event_data("plotly_hover") 
      if (is.null(event1)) "Hover on a point!" else event1    
  }) # end of output$event block      
    

} #end of server block

shinyApp(ui, server)
