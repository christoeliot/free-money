library(shiny)

ui <- fluidPage(# Application title
  titlePanel('Daily Fantasy Roster Optimization'),
  
  # change to grid layout for more flexibility  
  sidebarLayout(sidebarPanel(
    selectInput(
      'sport',
      'Select Sport:',
      c('Baseball', 'Football', 'Basketball')
    ),
    dateInput(
      'date',
      'Select Contest Date:'
    ),
    fileInput(
      'file',
      'Upload players csv from DraftKings.'
    ),
    sliderInput(
      'lineups',
      'Number of lineups:',
      min = 1,
      max = 25,
      value = 5
    ),
    actionButton(
      'goButton',
      'Generate Optimal Lineups'
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput('boxPlot'))))

server <- function(input, output) {
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is 'reactive' and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    ###### this is not real
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
  })
}

shinyApp(ui = ui, server = server)