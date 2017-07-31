library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Exploring effects of LD - 3"),
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("time", "Aage range", min = 1, max = 120, value = c(30,105)),
             sliderInput("t3d","Age", min = 1,max = 120,value = 60, step=1)
           ),
           wellPanel(
             h4("Constants"),
             sliderInput("Q11","Q11:", min = 0,max = 2,value = 0.5e-1, step=1e-5),
             sliderInput("Q12", "Q12:", min = 0, max = 1, value = 0.5e-2, step=1e-6),
             sliderInput("Q22", "Q22:", min = 0, max = 2, value = 0.5e-1, step=1e-5),
             sliderInput("g01", "g01:", min = 0, max = 1, value = 0, step = 1),
             sliderInput("g02", "g02:", min = 0, max = 1, value = 1, step=1)
             ),
           wellPanel(
             h4("Initial values"),
             sliderInput("m10","m1(0):", min = 0,max = 5,value = 0, step=1e-4),
             sliderInput("m20", "m2(0):", min = 0, max = 5, value = 0, step=1e-4),
             sliderInput("gamma110", "gamma11(0):", min = 0, max = 2, value = 0.5, step=1e-4),
             sliderInput("gamma120", "gamma12(0):", min = 0, max = 2, value = 0.1, step=1e-4),
             sliderInput("gamma220", "gamma22(0):", min = 0, max = 2, value = 0.5, step=1e-4)
           )
           
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Main Plots", 
                 plotOutput("distPlot",height = 1024),
                 downloadButton("savePlotMain", label="Save")),
        tabPanel("Contour plot", 
                 plotOutput("countourPlot", height = 1024),
                 downloadButton("savePlotContour", label="Save")),
        tabPanel("3D density plot", 
                 plotOutput("distPlot3d",height = 1024),
                 downloadButton("savePlotDensity", label="Save"))
      )
      
    )
  )
))