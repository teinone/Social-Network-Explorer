# ui
# ---------------------------------------------------------------------------------
# Orlando Shooting
# ---------------------------------------------------------------------------------
# Histogram
tabPanel("No. of posts over time",
         titlePanel("Orlando Shooting"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Orlando_His", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.histogram.orlando")
           ) # mainPanel end
         ), # sidebarLayout end
         p("The event happens on 2016-06-12.")
) # end of tabPanel

# Tiffany's compound graph
tabPanel("Sentiment analysis I",
         titlePanel("Orlando Shooting"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Orlando_Combo", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.combo.orlando")
           ) # mainPanel end
         ) # sidebarLayout end
) # end of tabPanel

# ---------------------------------------------------------------------------------
# Ferguson Unrest
# ---------------------------------------------------------------------------------
# Histogram
tabPanel("No. of posts over time", 
         titlePanel("Ferguson Unrest"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Ferguson_His", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.histogram.ferguson")
           ) # mainPanel end
         ), # sidebarLayout end
         p("The event happens on 2014-08-09.")
) # end of tabPanel

# Tiffany's compound graph
tabPanel("Sentiment analysis I",
         titlePanel("Ferguson Unrest"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Ferguson_Combo", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.combo.ferguson")
           ) # mainPanel end
         ) # sidebarLayout end
) # end of tabPanel

# ---------------------------------------------------------------------------------
# Obama visiting Cuba
# ---------------------------------------------------------------------------------
# Histogram
tabPanel("No. of posts over time", 
         titlePanel("Obama Visiting Cuba"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Obama_His", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.histogram.obama")
           ) # mainPanel end
         ), # sidebarLayout end
         p("The event happens on 2016-03-24.")
) # end of tabPanel

# Tiffany's compound graph
tabPanel("Sentiment analysis I", 
         titlePanel("Obama Visiting Cuba"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Obama_Combo", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.combo.obama")
           ) # mainPanel end
         ), # sidebarLayout end
         p("The event happens on 2016-12-01.")
) # end of tabPanel

# ---------------------------------------------------------------------------------
# Trump named TIME's Person of the Year
# ---------------------------------------------------------------------------------
# Histogram
tabPanel("No. of posts over time", 
         titlePanel("Trump named TIME's Person of the Year"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Trump_His", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.histogram.trump")
           ) # mainPanel end
         ) # sidebarLayout end
)# end of tabPanel

# Tiffany's compound graph
tabPanel("Sentiment analysis I", 
         titlePanel("Trump named TIME's Person of the Year"),
         sidebarLayout(
           sidebarPanel(
             strong("Selection:"),
             selectInput("Time_Selection_Trump_Combo", 
                         label = "Choose the time period of the event",
                         choices = c("Before", 
                                     "After",
                                     "Entire"
                                    ), # end of choices
                         selected = "Before"
                         ) # end of selectInput
           ), # SliderPanel end
           mainPanel(
             plotOutput("plot.combo.trump")
           ) # mainPanel end
         ) # sidebarLayout end
) # end of tabPanel

# server

# ---------------------------------------------------------------------------------
# Plot histogram plot for orlando shooting
# ---------------------------------------------------------------------------------
filterData.orlando.his <- reactive({
  his.orlando <- switch(input$Time_Selection_Orlando_His, 
                        "Before" = f.event.hist(dt.r.orlando.15d.b, "Before"),
                        "After" = f.event.hist(dt.r.orlando.15d.a, "After"), 
                        "Entire" = f.event.hist(dt.r.orlando.15d.f, "Entire")
                        ) # end of switch
  return(his.orlando)
}) # end of filterData.orlando.his
output$plot.histogram.orlando <- renderPlot({
  his.orlando <- filterData.orlando.his()
  his.orlando
}) # end of renderPlot

# ---------------------------------------------------------------------------------
# Plot histogram plot for Ferguson Unrest
# ---------------------------------------------------------------------------------
filterData.ferguson.his <- reactive({
  his.ferguson <- switch(input$Time_Selection_Ferguson_His, 
                           "Before" = f.event.hist(dt.r.ferguson.15d.b, "Before"),
                           "After" = f.event.hist(dt.r.ferguson.15d.a, "After"), 
                           "Entire" = f.event.hist(dt.r.ferguson.15d.f, "Entire")
                        ) # end of switch
  return(his.ferguson)
}) # filterData.ferguson.his
output$plot.histogram.ferguson <- renderPlot({
  his.ferguson <- filterData.ferguson.his()
  his.ferguson
}) # end of renderPlotly

# ---------------------------------------------------------------------------------
# Plot histogram plot for Obama visiting Cuba
# ---------------------------------------------------------------------------------
filterData.obama.his <- reactive({
  his.obama <- switch(input$Time_Selection_Obama_His, 
                         "Before" = f.event.hist(dt.r.obama.15d.b, "Before"),
                         "After" = f.event.hist(dt.r.obama.15d.a, "After"), 
                         "Entire" = f.event.hist(dt.r.obama.15d.f, "Entire")
                      ) # end of switch
  return(his.obama)
}) # end of filterData.obama.his
output$plot.histogram.obama <- renderPlot({
  his.obama <- filterData.obama.his()
  his.obama
}) # end of renderPlotly

# ---------------------------------------------------------------------------------
# Plot histogram plot for Trump
# ---------------------------------------------------------------------------------
filterData.trump.his <- reactive({
  his.trump <- switch(input$Time_Selection_Trump_His, 
                      "Before" = f.event.hist(dt.r.trump.15d.b, "Before"),
                      "After" = f.event.hist(dt.r.trump.15d.a, "After"), 
                      "Entire" = f.event.hist(dt.r.trump.15d.f, "Entire")
                     ) # end of switch
  return(his.trump)
}) # end of filterData.trump.his
output$plot.histogram.trump <- renderPlot({
  his.trump <- filterData.trump.his()
  his.trump
}) # end of renderPlotly


# ---------------------------------------------------------------------------------
# Plot combo plot for orlando shooting
# ---------------------------------------------------------------------------------
filterData_orlando_combo <- reactive({
  combo.orlando <- switch(input$Time_Selection_Orlando_Combo, 
                          "Before" = f.combochart(dt.r.orlando.15d.b),
                          "After" = f.combochart(dt.r.orlando.15d.a), 
                          "Entire" = f.combochart(dt.r.orlando.15d.f)
                          ) # end of switch
  return(combo.orlando)
}) # end of filterData_orlando_combo
output$plot.combo.orlando <- renderPlot({
  combo.orlando <- filterData_orlando_combo()
  combo.orlando
}) # end of renderPlotly

# ---------------------------------------------------------------------------------
# Plot combo plot for Ferguson Unrest
# ---------------------------------------------------------------------------------
filterData.ferguson.combo <- reactive({
  combo.ferguson <- switch(input$Time_Selection_Ferguson_Combo, 
                           "Before" = f.combochart(dt.r.ferguson.15d.b),
                           "After" = f.combochart(dt.r.ferguson.15d.a), 
                           "Entire" = f.combochart(dt.r.ferguson.15d.f)
                          ) # end of switch
  return(combo.ferguson)
}) # end of filterData.ferguson.combo
output$plot.combo.ferguson <- renderPlot({
  combo.ferguson <- filterData.ferguson.combo()
  combo.ferguson
}) # end of renderPlotly

# ---------------------------------------------------------------------------------
# Plot combo plot for Obama Visiting Cuba
# ---------------------------------------------------------------------------------
filterData.obama.combo <- reactive({
  combo.obama <- switch(input$Time_Selection_Obama_Combo, 
                           "Before" = f.combochart(dt.r.obama.15d.b),
                           "After" = f.combochart(dt.r.obama.15d.a), 
                           "Entire" = f.combochart(dt.r.obama.15d.f)
                        ) # end of switch
  return(combo.obama)
}) # end of filterData.obama.combo
output$plot.combo.obama <- renderPlot({
  combo.obama <- filterData.obama.combo()
  combo.obama
}) # end of renderPlotly

# ---------------------------------------------------------------------------------
# Plot combo plot for Trump named TIME's Person of the Year
# ---------------------------------------------------------------------------------
filterData.trump.combo <- reactive({
  combo.trump <- switch(input$Time_Selection_Trump_Combo, 
                           "Before" = f.combochart(dt.r.trump.15d.b),
                           "After" = f.combochart(dt.r.trump.15d.a), 
                           "Entire" = f.combochart(dt.r.trump.15d.f)
                        ) # end of switch
  return(combo.trump)
}) # end of filterData.trump.combo
output$plot.combo.trump <- renderPlot({
  combo.trump <- filterData.trump.combo()
  combo.trump
}) # end of renderPlotly