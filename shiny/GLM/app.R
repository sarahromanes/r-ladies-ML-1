#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

data <- read.csv("../../data/Spiders.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Logistic Regression - what is the optimal curve?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      HTML(
        "Use the sliders to adjust the values of a,b, and c, for the covariance matrix. The resultant output
        is a scatter plot of the datapoints generated from a MVN(0, Sigma) distribution, as well as a visualisation of the effect of the linear transformation of the Identity matrix by Sigma. Fun fact - the area plotted is the determinant of Sigma!"),
      # Text instructions
      img(src="covariance.png",width="100%"),
      
      sliderInput("intercept",
                  withMathJax(helpText("The value of the intercept, $$\\beta_0$$")),
                  min = -10,
                  max = 0,
                  value = -8),
      sliderInput("slope",
                  withMathJax(helpText("The value of the slope, $$\\beta_1$$")),
                  min = 5,
                  max = 20,
                  value = 15)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("glmPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$glmPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    coef <- c(input$slope, input$intercept)
    link <- coef[2] + coef[1]*data$GrainSize
    Fitted <- 1/(1+exp(-link))
    Spider <- transform(data, Fitted = Fitted)
    
    
    # draw the histogram with the specified number of bins
    ggplot(Spider, aes(GrainSize, Spiders)) + 
      geom_point(color="red") + 
      geom_line(aes(GrainSize, Fitted), col="dodgerblue") +
      geom_segment(aes(x = GrainSize, y = Spiders,
                       xend = GrainSize, yend = Fitted)) +
      coord_cartesian(xlim = c(0.2, 1.1), ylim = c(0, 1))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

