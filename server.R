library(shiny)
library(grid)
library(extrafont)
library(deSolve)
library(MASS)
source("multiplot.R")

# Define server logic #
shinyServer(function(input, output, session) {
  
  
  func <- function(t, y, parms) 
  {
    Q11 <- parms[1]
    Q12 <- parms[2]
    Q21 <- parms[3]
    Q22 <- parms[4]
    g01 <- parms[5]
    g02 <- parms[6]
    
    m1 <- y[1]
    m2 <- y[2]
    gamma11 <- y[3]
    gamma12 <- y[4]
    gamma21 <- gamma12
    gamma22 <- y[5]
    
    list(c(
      -2*( (gamma11*Q11 + gamma12*Q21)*(m1 - g01) +    #dm1/dt
             (gamma11*Q12 + gamma12*Q22)*(m2 - g02) ), 
      -2*( (gamma21*Q11 + gamma22*Q21)*(m1 - g01) +    #dm2/dt
             (gamma21*Q12 + gamma22*Q22)*(m2 - g02) ), 
      -2*( gamma11*Q11*gamma11 + gamma12*Q21*gamma11 +   #dgamma11/dt
             gamma11*Q12*gamma21 + gamma12*Q22*gamma21 ),
      -2*( gamma11*Q11*gamma12 + gamma12*Q21*gamma12 +   #dgamma12/dt
             gamma11*Q12*gamma22 + gamma12*Q22*gamma22 ), 
      -2*( gamma21*Q11*gamma12 + gamma22*Q21*gamma12 +   #dgamma22/dt
             gamma21*Q12*gamma22 + gamma22*Q22*gamma22 )
    ))
  }
  
  calculate <- function() 
  {
    
  }
  
  mPlot <- function(cols=1, save=F){
    
    yini <- c(y1 = input$m10, y2 = input$m20, y3 = input$gamma110, y4 = input$gamma120, y5 = input$gamma220)
    res <- ode(y = yini, func = func,
               times = input$time[1]:input$time[2], parms = c(input$Q11, input$Q12, input$Q21, input$Q22, 
                                                              input$g01, input$g02))
    
    colnames(res) <- c("time", "m1", "m2", "gamma11", "gamma12", "gamma22")
    #multiplot(res[,2],res[,3],
    #          cols=cols, title="", titlesize=12,titlefont="Courier", titleface=2)
    plot(res)
  }
  
  mPlot2 <- function(cols=1, save=F){
    t <- input$t3d
    yini <- c(y1 = input$m10, y2 = input$m20, y3 = input$gamma110, y4 = input$gamma120, y5 = input$gamma220)
    res <- ode(y = yini, func = func,
               times = input$time[1]:input$time[2], parms = c(input$Q11, input$Q12, input$Q21, input$Q22, 
                                                              input$g01, input$g02))
    
    colnames(res) <- c("time", "m1", "m2", "gamma11", "gamma12", "gamma22")
    #multiplot(res[,2],res[,3],
    #          cols=cols, title="", titlesize=12,titlefont="Courier", titleface=2)
    
    bivn <- mvrnorm(1000, mu = c(res[t,1], res[t,2]), 
                    Sigma = matrix(c(res[t,3], res[t,4], res[t,4], res[t,5]), 2))
    bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 100)
    col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
    #persp3d(x=bivn.kde, col=col1)
    persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)
  }

  
  output$distPlot <- renderPlot({
    print(mPlot())
  })
  
  output$distPlot3d <- renderPlot({
    print(mPlot2())
  })

  
})