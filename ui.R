library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Exploring effects of LD - 3"),
  fluidRow(
    column(4,
           wellPanel(
             h4("Time (age range)"),
             sliderInput("time",
                  "t:",
                  min = 1,
                  max = 120,
                  value = c(30,105))),
           wellPanel(
             h4("T: time for 3D plot and number of samples: N"),
             sliderInput("t3d","t:", min = 1,max = 120,value = 50, step=1),
             sliderInput("N","N:", min = 1,max = 10000,value = 1000, step=1)
           ),
           wellPanel(
             h4("Constant values"),
             sliderInput("Q11","Q11:", min = 0,max = 1e-3,value = 0.5e-3, step=1e-5),
             sliderInput("Q12", "Q12:", min = 0, max = 1e-4, value = 0.5e-4, step=1e-6),
             sliderInput("Q21", "Q21:", min = 0, max = 1e-4, value = 0.5e-4, step=1e-6),
             sliderInput("Q22", "Q22:", min = 0, max = 1e-3, value = 0.5e-3, step=1e-5),
             sliderInput("g01", "g01:", min = 0, max = 1, value = 0, step = 1),
             sliderInput("g02", "g02:", min = 0, max = 1, value = 1, step=1)
             ),
           wellPanel(
             h4("Initial values"),
             sliderInput("m10","m1(0):", min = 0,max = 5,value = 1),
             sliderInput("m20", "m2(0):", min = 0, max = 5, value = 2),
             sliderInput("gamma110", "gamma11(0):", min = 0, max = 1, value = 0.5),
             sliderInput("gamma120", "gamma12(0):", min = 0, max = 1, value = 0.05),
             sliderInput("gamma220", "gamma22(0):", min = 0, max = 1, value = 0.5)
           )
           
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Main Plots", 
                 plotOutput("distPlot",height = 1024)),
        tabPanel("3D density plot from 2D bivariate normal distribution", 
                 plotOutput("distPlot3d",height = 1024))
      )
      
    )
  )
))