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
            menuItem("Network", tabName = "Network",   icon = icon("th")),
            menuItem("Kurzegezagt!", tabName = "Description", icon = icon("th"))
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
        ), # first tab end
      
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
          )#box end
        ) #fluidRow end
      ),# second tab item end
      
      # Start of the 3rd tab 
      tabItem(tabName = "Description",
               fluidRow(
                 # Tab.3, box.1
                 box(
                   verbatimTextOutput("description")
                 ), # end of the box.1
                 
                 # Start of the 2nd box 
                 box(
                   verbatimTextOutput("instruction")
                 ) #end of tab.3, box.2
                         ) #end of the fluidrow 
        
      ) #End of the 3rd tab
       
      
      
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
    
   output$description <- renderText({ "The hyperlink network represents the directed connections between two subreddits (a subreddit is a community on Reddit). We also provide subreddit embeddings. The network is extracted from publicly available Reddit data of 2.5 years from Jan 2014 to April 2017.

     Subreddit Hyperlink Network: the subreddit-to-subreddit hyperlink network is extracted from the posts that create hyperlinks from one subreddit to another. We say a hyperlink originates from a post in the source community and links to a post in the target community. Each hyperlink is annotated with three properties: the timestamp, the sentiment of the source community post towards the target community post, and the text property vector of the source post. The network is directed, signed, temporal, and attributed.
     
     Note that each post has a title and a body. The hyperlink can be present in either the title of the post or in the body. Therefore, we provide one network file for each."
     })   # end of the description text 

   
   output$insruction <- renderText({"test instruction"
     }) # end of instruction text 
} #end of server block

shinyApp(ui, server)
