library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Exploring effects of LD - 3"),
  fluidRow(
    column(4,
           wellPanel(
             
             h4("Time"),
             sliderInput("time",
                  "t:",
                  min = 1,
                  max = 120,
                  value = c(30,105))),
           wellPanel(
             h4("Constant values"),
             sliderInput("Q11","Q11:", min = 0,max = 0.5,value = 0.05, step=1e-3),
             sliderInput("Q12", "Q12:", min = 0, max = 0.1, value = 0.005, step=1e-4),
             sliderInput("Q21", "Q21:", min = 0, max = 0.1, value = 0.005, step=1e-4),
             sliderInput("Q22", "Q22:", min = 0, max = 0.5, value = 0.05, step=1e-3),
             sliderInput("g01", "g01:", min = 0, max = 1, value = 0, step = 1),
             sliderInput("g02", "g02:", min = 0, max = 1, value = 1, step=1)
             ),
           wellPanel(
             h4("Initial values"),
             sliderInput("m10","m1(0):", min = 0,max = 5,value = 1),
             sliderInput("m20", "m2(0):", min = 0, max = 5, value = 2),
             sliderInput("gamma110", "gamma11(0):", min = 0, max = 0.1, value = 0.05),
             sliderInput("gamma120", "gamma12(0):", min = 0, max = 0.1, value = 0.005),
             sliderInput("gamma220", "gamma22(0):", min = 0, max = 0.1, value = 0.05)
           ),
           wellPanel(
             h4("Time for 3D plot"),
             sliderInput("t3d","t:", min = 1,max = 120,value = 1, step=1)
           )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Main Plots", 
                 plotOutput("distPlot",height = 1024)),
        tabPanel("3D Plots", 
                 plotOutput("distPlot3d",height = 1024))
      )
      
    )
  )
))