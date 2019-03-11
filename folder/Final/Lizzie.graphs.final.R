#working one 
# Load the necessary libraries
library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)     
library(ggplot2)        
library(stringr)
#library(dygraphs)
library(plyr)
library(scales)
library(plotly)
library(shinyWidgets)
#library(dashboardthemes)
library(igraph)
library(data.table)
# Change working directory
getwd()
setwd("/Users/Desktop/NDATeam12/shiny/")
load("/Users/Desktop/NDATeam12/reddit.masters.RData")

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
ui <-
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
      #theme_blue_gradient,
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
                      p("Obama's visit to Cuba marks the first visit of an American president since 2028. It marks a huge change in relationship between U.S. and Cuba.")
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
                                tabPanel("Sentiment analysis II", plotlyOutput("sentiment.distribution.orlando"),
                                         p("Remember to set graph width = '100%'")
                                ),# end of tabPanel
                                
                                
                                # Minxin's graph
                                tabPanel("Sentiment analysis III", plotlyOutput("plotName4"),
                                         p("Please  input summary here, and insert graph above."), 
                                         p("Remember to set graph width = '100%'")
                                ),#end of tabPanel
                                # Subreddit network
                                tabPanel("Subreddit network", plotOutput("plotName5"),
                                         p("Please  input summary here, and insert graph above."), 
                                         p("Remember to set graph width = '100%'")
                                ),#end of tabPanel
                                # Degree distribution
                                tabPanel("Degree distribution", plotOutput("degree.distribution.orlando"),
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
                                tabPanel("Sentiment analysis II", plotlyOutput("sentiment.distribution.ferguson"),
              
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
                                tabPanel("Degree distribution", plotOutput("degree.distribution.ferguson"),
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
                                tabPanel("Sentiment analysis II", plotlyOutput("sentiment.distribution.obama"),
                              
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
                                # Lizzie's Degree distribution
                                tabPanel("Degree distribution", plotOutput("degree.distribution.obama"),
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
                                tabPanel("No. of posts over time", plotOutput("plotName20"),
                                         p("Please  input summary here, and insert graph above."), 
                                         p("Remember to set graph width = '100%'")
                                ),# end of tabPanel
                                # Tiffany's compound graph
                                tabPanel("Sentiment analysis I", verbatimTextOutput("summary20"),
                                         p("Please  input summary here, and insert graph above."), 
                                         p("Remember to set graph width = '100%'")
                                ),# end of tabPanel
                                # Lizzie's sentiment distribution graph
                                tabPanel("Sentiment analysis II", plotlyOutput("sentiment.distribution.trump"),
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
                                tabPanel("Degree distribution", plotOutput("degree.distributiom.trump"),
                                      
                                         p("Remember to set graph width = '100%'")
                                )#end of tabPanel
                                
                    )#end of tabsetPanel
                  ) #box end
                ) #fluidRow end
        ) #tabItem end
        
      ) #tabItems end
    )#dashboard Body end
  ) #dashboard Page end
#shinyUI end


# start Lizzie's server end 
server <- function(input, output){
  output$sentiment.distribution.ferguson <- renderPlotly({
    dt.ferguson.sentiment <- dt.reddit.ferguson[,list(unique(SOURCE_SUBREDDIT))]
    
    setnames(dt.ferguson.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_ferguson <- setDT(dt.reddit.ferguson)[, .(total_sentiment_ferguson = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.ferguson.sentiment <- merge(dt.reddit.ferguson, total_sentiment_ferguson, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.ferguson <- graph.data.frame(dt.reddit.ferguson, directed= TRUE)
    V(g.ferguson)$ag_sentiment <- as.numeric(dt.ferguson.sentiment$total_sentiment_ferguson[match(V(g.ferguson)$name,dt.reddit.ferguson$SOURCE_SUBREDDIT)])
    # Agregated sentiment and most connected component
    cl.ferguson <- clusters(g.ferguson)
    g.biggest.component.ferguson <- induced.subgraph(g.ferguson, which(cl.ferguson$membership == which.max(cl.ferguson$csize)))
    
    # Freq for each sentiment
    dt.component.sentiment.ferguson <- as.data.table(table((V(g.biggest.component.ferguson)$ag_sentiment)))
    
    
    # Graph: Distribution of emotion for most active users
    plot.sentiment.ferguson <- 
      ggplot(dt.component.sentiment.ferguson, aes(x=N, y=V1)) + 
      geom_point(size= ifelse(dt.component.sentiment.ferguson$V1 < 0.5 & dt.component.sentiment.ferguson$V1 > -0.5, 0, 
                              ifelse(dt.component.sentiment.ferguson$V1 < 1.5 & dt.component.sentiment.ferguson$V1 > -1.5, 1,
                                     ifelse(dt.component.sentiment.ferguson$V1 < 2.5 & dt.component.sentiment.ferguson$V1 > -2.5, 2, 3
                                     ))), 
                 col = ifelse(dt.component.sentiment.ferguson$V1 < 0, "deepskyblue", "tomato")
      ) + 
      geom_line() + 
      geom_smooth(se=F) + 
      xlab("Frequency") + 
      ylab("Sentiment") +
      scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.ferguson$N), max(dt.component.sentiment.ferguson$N), by = 30), 1)) +
      scale_y_discrete(breaks = c(min(dt.component.sentiment.ferguson$V1), 0, 5, max(dt.component.sentiment.ferguson$V1)))
    plot.sentiment.ferguson
  }) # end of renderPlot, sentiment.distribution.ferguson
  
  #start of sentiment.distribution.orlando
  output$sentiment.distribution.orlando <- renderPlotly({
    dt.orlando.sentiment <- dt.reddit.orlando[,list(unique(SOURCE_SUBREDDIT))]
    
    setnames(dt.orlando.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_orlando <- setDT(dt.reddit.orlando)[, .(total_sentiment_orlando = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.orlando.sentiment <- merge(dt.reddit.orlando, total_sentiment_orlando, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.orlando <- graph.data.frame(dt.reddit.orlando, directed= TRUE)
    V(g.orlando)$ag_sentiment <- as.numeric(dt.orlando.sentiment$total_sentiment_orlando[match(V(g.orlando)$name,dt.reddit.orlando$SOURCE_SUBREDDIT)])
    # Agregated sentiment and most connected component
    cl.orlando <- clusters(g.orlando)
    g.biggest.component.orlando <- induced.subgraph(g.orlando, which(cl.orlando$membership == which.max(cl.orlando$csize)))
    
    # Freq for each sentiment
    dt.component.sentiment.orlando <- as.data.table(table((V(g.biggest.component.orlando)$ag_sentiment)))
    
    
    # Graph: Distribution of emotion for most active users
    plot.sentiment.orlando <- 
      ggplot(dt.component.sentiment.orlando, aes(x=N, y=V1)) + 
      geom_point(size= ifelse(dt.component.sentiment.orlando$V1 < 0.5 & dt.component.sentiment.orlando$V1 > -0.5, 0, 
                              ifelse(dt.component.sentiment.orlando$V1 < 1.5 & dt.component.sentiment.orlando$V1 > -1.5, 1,
                                     ifelse(dt.component.sentiment.orlando$V1 < 2.5 & dt.component.sentiment.orlando$V1 > -2.5, 2, 3
                                     ))), 
                 col = ifelse(dt.component.sentiment.orlando$V1 < 0, "deepskyblue", "tomato")
      ) + 
      geom_line() + 
      geom_smooth(se=F) + 
      xlab("Frequency") + 
      ylab("Sentiment") +
      scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.orlando$N), max(dt.component.sentiment.orlando$N), by = 30), 1)) +
      scale_y_discrete(breaks = c(min(dt.component.sentiment.orlando$V1), 0, 5, max(dt.component.sentiment.orlando$V1)))
    plot.sentiment.orlando
  }) # end of renderPlot, sentiment.distribution.orlando
  
  # start of sentiment.distribution.trump
  output$sentiment.distribution.trump <- renderPlotly({
    dt.trump.sentiment <- dt.reddit.trump[,list(unique(SOURCE_SUBREDDIT))]
    
    setnames(dt.trump.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_trump <- setDT(dt.reddit.trump)[, .(total_sentiment_trump = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.trump.sentiment <- merge(dt.reddit.trump, total_sentiment_trump, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.trump <- graph.data.frame(dt.reddit.trump, directed= TRUE)
    V(g.trump)$ag_sentiment <- as.numeric(dt.trump.sentiment$total_sentiment_trump[match(V(g.trump)$name,dt.reddit.trump$SOURCE_SUBREDDIT)])
    # Agregated sentiment and most connected component
    cl.trump <- clusters(g.trump)
    g.biggest.component.trump <- induced.subgraph(g.trump, which(cl.trump$membership == which.max(cl.trump$csize)))
    
    # Freq for each sentiment
    dt.component.sentiment.trump <- as.data.table(table((V(g.biggest.component.trump)$ag_sentiment)))
    
    
    # Graph: Distribution of emotion for most active users
    plot.sentiment.trump <- 
      ggplot(dt.component.sentiment.trump, aes(x=N, y=V1)) + 
      geom_point(size= ifelse(dt.component.sentiment.trump$V1 < 0.5 & dt.component.sentiment.trump$V1 > -0.5, 0, 
                              ifelse(dt.component.sentiment.trump$V1 < 1.5 & dt.component.sentiment.trump$V1 > -1.5, 1,
                                     ifelse(dt.component.sentiment.trump$V1 < 2.5 & dt.component.sentiment.trump$V1 > -2.5, 2, 3
                                     ))), 
                 col = ifelse(dt.component.sentiment.trump$V1 < 0, "deepskyblue", "tomato")
      ) + 
      geom_line() + 
      geom_smooth(se=F) + 
      xlab("Frequency") + 
      ylab("Sentiment") +
      scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.trump$N), max(dt.component.sentiment.trump$N), by = 30), 1)) +
      scale_y_discrete(breaks = c(min(dt.component.sentiment.trump$V1), 0, 5, max(dt.component.sentiment.trump$V1)))
    plot.sentiment.trump
  }) # end of renderPlot, sentiment.distribution.trump
  #start of sentiment.distribution.obama 
  output$sentiment.distribution.obama <- renderPlotly({
    dt.obama.sentiment <- dt.reddit.obama[,list(unique(SOURCE_SUBREDDIT))]
    
    setnames(dt.obama.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_obama <- setDT(dt.reddit.obama)[, .(total_sentiment_obama = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.obama.sentiment <- merge(dt.reddit.obama, total_sentiment_obama, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.obama <- graph.data.frame(dt.reddit.obama, directed= TRUE)
    V(g.obama)$ag_sentiment <- as.numeric(dt.obama.sentiment$total_sentiment_obama[match(V(g.obama)$name,dt.reddit.obama$SOURCE_SUBREDDIT)])
    # Agregated sentiment and most connected component
    cl.obama <- clusters(g.obama)
    g.biggest.component.obama <- induced.subgraph(g.obama, which(cl.obama$membership == which.max(cl.obama$csize)))
    
    # Freq for each sentiment
    dt.component.sentiment.obama <- as.data.table(table((V(g.biggest.component.obama)$ag_sentiment)))
    # Graph: Distribution of emotion for most active users
    plot.sentiment.obama <- 
      ggplot(dt.component.sentiment.obama, aes(x=N, y=V1)) + 
      geom_point(size= ifelse(dt.component.sentiment.obama$V1 < 0.5 & dt.component.sentiment.obama$V1 > -0.5, 0, 
                              ifelse(dt.component.sentiment.obama$V1 < 1.5 & dt.component.sentiment.obama$V1 > -1.5, 1,
                                     ifelse(dt.component.sentiment.obama$V1 < 2.5 & dt.component.sentiment.obama$V1 > -2.5, 2, 3
                                     ))), 
                 col = ifelse(dt.component.sentiment.obama$V1 < 0, "deepskyblue", "tomato")
      ) + 
      geom_line() + 
      geom_smooth(se=F) + 
      xlab("Frequency") + 
      ylab("Sentiment") +
      scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.obama$N), max(dt.component.sentiment.obama$N), by = 30), 1)) +
      scale_y_discrete(breaks = c(min(dt.component.sentiment.obama$V1), 0, 5, max(dt.component.sentiment.obama$V1)))
    plot.sentiment.obama
  }) # end of renderPlot, sentiment.distribution.obama
  
  output$degree.distribution.ferguson <- renderPlot({ # start degree.distribution.ferguson
    dt.ferguson.sentiment <- dt.reddit.ferguson[,list(unique(SOURCE_SUBREDDIT))]
    setnames(dt.ferguson.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_ferguson <- setDT(dt.reddit.ferguson)[, .(total_sentiment_ferguson = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.ferguson.sentiment <- merge(dt.reddit.ferguson, total_sentiment_ferguson, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.ferguson <- graph.data.frame(dt.reddit.ferguson, directed= TRUE)
    V(g.ferguson)$ag_sentiment <- as.numeric(dt.ferguson.sentiment$total_sentiment_ferguson[match(V(g.ferguson)$name,dt.reddit.ferguson$SOURCE_SUBREDDIT)])
    
    degree.dist.ferguson <- degree_distribution(g.ferguson, cumulative=T, mode="all")
    plot.degree.distribution.ferguson <-
      plot(x=0:max(degree(g.ferguson)), y=1-degree.dist.ferguson, pch=20, cex=1.2, col="orange", 
            xlab="Degree", ylab="Frequency")
    plot.degree.distribution.ferguson
    
  }) #end degree.distribution.ferguson
  
  #start of degree.distribution.orlando
  output$degree.distribution.orlando <- renderPlot({ # start degree.distribution.orlando
    dt.orlando.sentiment <- dt.reddit.orlando[,list(unique(SOURCE_SUBREDDIT))]
    setnames(dt.orlando.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_orlando <- setDT(dt.reddit.orlando)[, .(total_sentiment_orlando = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.orlando.sentiment <- merge(dt.reddit.orlando, total_sentiment_orlando, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.orlando <- graph.data.frame(dt.reddit.orlando, directed= TRUE)
    V(g.orlando)$ag_sentiment <- as.numeric(dt.orlando.sentiment$total_sentiment_orlando[match(V(g.orlando)$name,dt.reddit.orlando$SOURCE_SUBREDDIT)])
    
    degree.dist.orlando <- degree_distribution(g.orlando, cumulative=T, mode="all")
    plot.degree.distribution.orlando <-
      plot(x=0:max(degree(g.orlando)), y=1-degree.dist.orlando, pch=20, cex=1.2, col="orange", 
            xlab="Degree", ylab="Frequency")
    plot.degree.distribution.orlando
    
  }) #end degree.distribution.orlando
  output$degree.distribution.trump <- renderPlot({ # start degree.distribution.trump
    dt.trump.sentiment <- dt.reddit.trump[,list(unique(SOURCE_SUBREDDIT))]
    setnames(dt.trump.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
    total_sentiment_trump <- setDT(dt.reddit.trump)[, .(total_sentiment_trump = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
    
    # Update Event 
    dt.trump.sentiment <- merge(dt.reddit.trump, total_sentiment_trump, by = "SOURCE_SUBREDDIT")
    
    #Plot Network degree distribution
    g.trump <- graph.data.frame(dt.reddit.trump, directed= TRUE)
    V(g.trump)$ag_sentiment <- as.numeric(dt.trump.sentiment$total_sentiment_trump[match(V(g.trump)$name,dt.reddit.trump$SOURCE_SUBREDDIT)])
    
    degree.dist.trump <- degree_distribution(g.trump, cumulative=T, mode="all")
    plot.degree.distribution.trump <-
      plot(x=0:max(degree(g.trump)), y=1-degree.dist.trump, pch=20, cex=1.2, col="orange", 
           xlab="Degree", ylab="Frequency")
    plot.degree.distribution.trump
    
  }) # end of degree.distribution.trump
  
  
}# end of server side 

shinyApp(ui, server)
