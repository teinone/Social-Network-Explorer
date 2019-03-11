shinyServer(function(input, output) {
  
  # ---------------------------------------------------------------------------------
  # Please *name* each function beforehand using this to make the codes more readable
  # Preferably same *name* as the *separate function file*
  # ---------------------------------------------------------------------------------
  
  # ---------------------------------------------------------------------------------
  # Example function
  # ---------------------------------------------------------------------------------
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
})#end of ShinyServer