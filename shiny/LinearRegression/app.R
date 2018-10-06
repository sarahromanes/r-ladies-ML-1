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

data <- read.csv("../../data/Income.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Linear Regression - what is the optimal line?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        HTML(
          "Use the sliders to adjust the values of the slope and interept for the linear model to describe the linear relationship between Income and Education for the Income dataset. The squares on the plot represent the square formed by the residual at each point, and the optimal line is given by the line which has the slope and intercept which minimises the sum of these squares."),
        # Text instructions
        img(src="rss.png",width="100%"),
        
         sliderInput("intercept",
                     withMathJax(helpText("The value of the intercept, $$\\beta_0$$")),
                     min = -50,
                     max = -30,
                     value = -39.5,
                     step=0.5),
         sliderInput("slope",
                     withMathJax(helpText("The value of the slope, $$\\beta_1$$")),
                     min = 1,
                     max = 10,
                     value = 5.5,
                     step=0.5)
         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("RSS"),
         br(),
         plotOutput("lmPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$lmPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     coef <- c(input$slope, input$intercept)
     Fitted <- coef[2] + coef[1]*data$Education
   
     Income <- transform(data, Fitted = Fitted, Resid = Income-Fitted)
     Pos <- Income[which(Income$Resid>0),]
     Neg <- Income[which(Income$Resid<0),]
     
     Pos <- transform(Pos, xr = Education- Resid)
     Neg <- transform(Neg, xr = Education +abs(Resid))
     
      
      # draw the histogram with the specified number of bins
     p <- ggplot(Income, aes(Education, Income)) + 
       geom_point(color="black") + 
       geom_abline(slope=coef[1], intercept=coef[2], col="darkorchid4") +
       geom_segment(aes(x = Education, y = Income,
                        xend = Education, yend = Fitted)) +
       geom_rect(data=Neg, aes(xmin=Education, xmax=xr, ymax=Fitted, ymin=Income), color="darkorchid1", fill="darkorchid1", alpha=0.3)+
       geom_rect(data=Pos, aes(xmin=xr, xmax=Education, ymax=Income, ymin=Fitted), color="darkorchid1", fill="darkorchid1", alpha=0.3)+
       coord_equal(ratio=1, ylim=c(15, 100), xlim=c(-10, 30))
     
     p + theme(axis.text = element_text(size = 20)) +theme_minimal()
   }, height=600, width=400)
   
   output$RSS <- renderText({
     coef <- c(input$slope, input$intercept)
     Fitted <- coef[2] + coef[1]*data$Education
     
     Income <- transform(data, Fitted = Fitted, Resid = Income-Fitted)
     rss <- round(sum((Income$Resid)^2),2)
     paste0("RSS = ", rss, ". This is the value of the sum of the squares on the plot, of which we are trying to minimise.")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

