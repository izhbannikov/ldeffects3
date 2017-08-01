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
    Q21 <- Q12
    Q22 <- parms[3]
    g01 <- parms[4]
    g02 <- parms[5]
    
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
  
  data <- reactive({
      
      maxval <- max(c(input$gamma110, input$gamma220))
      if(input$gamma120 > maxval)
      {
          updateSliderInput(session, "gamma120", value=maxval)
      }
      
      if(input$t3d > input$time[2])
      {
        updateSliderInput(session, "t3d", value=input$time[2])
      }
      
      if(input$t3d < input$time[1])
      {
        updateSliderInput(session, "t3d", value=input$time[1])
      }
    
      yini <- c(y1 = input$m10, y2 = input$m20, y3 = input$gamma110, y4 = input$gamma120, y5 = input$gamma220)
      res <- ode(y = yini, func = func,
               times = input$time[1]:input$time[2], parms = c(input$Q11, input$Q12, input$Q22, 
                                                              input$g01, input$g02))
    
      colnames(res) <- c("time", "m1", "m2", "gamma11", "gamma12", "gamma22")
      
      t <- input$t3d - input$time[1] + 1
      if(t < 0 | t > input$time[2] - input$time[1] + 1)
      {
        stop("Error: t out of age range!")
      }
      
      mu <- c(res[t,2], res[t,3])
      Sigma <- matrix(c(res[t,4], res[t,5], 
                        res[t,5], res[t,6]), 2)
      
      
      mu_x <- mu[1]
      mu_y <- mu[2]
      sigma_x <- (Sigma[1,1])^0.5
      sigma_y <- (Sigma[2,2])^0.5
      sigma_xy <- Sigma[1,2]
      
      rho <- Sigma[1,2]/(sigma_x*sigma_y)
      
      xx <- seq(-3,3,length.out = 100)
      yy <- seq(-3,3,length.out = 100)
      
      z <- matrix(nrow=length(xx), ncol=length(yy), NA)
      
      for(i in 1:length(xx))
      {
          for(j in 1:length(yy))
          {
              x <- xx[i]
              y <- yy[j]
              fxy <- 1/(2*pi*sigma_x*sigma_y*(1-rho^2)^0.5)*exp((-1/2)*((x - mu_x)^2/sigma_x^2 + (y - mu_y)^2/sigma_y^2 - 2*rho*(x - mu_x)*(y - mu_y)/(sigma_x*sigma_y)))
              z[i,j] <- fxy
          }
      }
      
      list(x=x,y=y,z=z,res=res)
      
  })
  
  PlotMain <- function(cols=1, save=F){
    res <- data()
    plot(res$res, cex.axis=1.5, cex.main=2, cex.lab=1.5, col="red", lwd=2)
  }
  
  PlotDensity <- function(cols=1, save=F){
    res <- data()
    persp(z=res$z, phi = 45, theta = 30, ticktype="detailed",
          xlab="x", ylab="y", zlab="z", axes=T)
  }

  
  PlotContour <- function(cols=1, save=F){
    res <- data()
    contour(z=res$z, col="red", labcex=1, lwd=2)
  }
  
  
  output$distPlot <- renderPlot({
    print(PlotMain())
  })
  
  output$distPlot3d <- renderPlot({
    print(PlotDensity())
  })
  
  output$countourPlot <- renderPlot({
      print(PlotContour())
  })
  
  output$savePlotMain <- downloadHandler(
    filename = "plotMain.png",
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotMain())
      dev.off()
    }
  ) 
  
  output$savePlotDensity <- downloadHandler(
    filename = "plotDensity.png",
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotDensity())
      dev.off()
    }
  )  
  
  output$savePlotContour <- downloadHandler(
    filename = "plotContour.png",
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotContour())
      dev.off()
    }
  )   
  

  
})