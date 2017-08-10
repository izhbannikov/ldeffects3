library(shiny)
library(grid)
library(extrafont)
library(deSolve)
library(MASS)
library(ggplot2)
library(pracma)
source("multiplot.R")

# Define server logic #
shinyServer(function(input, output, session) {
  
  getPlotTitle <- function()
  {
    paste("Q11 =",input$Q11, "Q12 =",input$Q12, "Q22 =",input$Q22, "g01 =",input$g01, "g02 =", input$g02, "\n",
                "gamma11(0) =", input$gamma110, "gamma12(0) =", input$gamma120, "gamma22(0) =", input$gamma220, "\n",
                "m1(0) =", input$m10, "m2(0) =",input$m20, "t1 = ", input$time[1], "t2 = ", input$time[2], "Age = ", input$t3d)
  }
  
  
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
      
      gamma.lim <- sqrt(input$gamma110*input$gamma220) #max(c(input$gamma110, input$gamma220))
      if(abs(input$gamma120) > gamma.lim)
      {
          updateSliderInput(session, "gamma120", value=gamma.lim)
      }
      
      if(input$t3d > input$time[2])
      {
        updateSliderInput(session, "t3d", value=input$time[2])
      }
      
      if(input$t3d < input$time[1])
      {
        updateSliderInput(session, "t3d", value=input$time[1])
      }
      
      q.lim <- sqrt(input$Q11*input$Q22) #max(c(input$gamma110, input$gamma220))
      if(abs(input$Q12) > q.lim)
      {
        updateSliderInput(session, "Q12", value=q.lim)
      }
      
      g01 <- input$g01
      g02 <- input$g02 
      Q01 <- input$Q01
      Q02 <- input$Q02
      Q11 <- input$Q11
      Q12 <- input$Q12
      Q22 <- input$Q22
      m10 <- input$m10
      m20 <- input$m20
      gamma110 <- input$gamma110
      gamma120 <- input$gamma120
      gamma220 <- input$gamma220
      
      yini <- c(y1 = m10, y2 = m20, y3 = gamma110, y4 = gamma120, y5 = gamma220)
      res <- ode(y = yini, func = func,
               times = input$time[1]:input$time[2], 
               parms = c(Q11, Q12, Q22, g01, g02))
    
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
      
      m1 <- res[,2]
      m2 <- res[,3]
      gamma11 <- res[,4] 
      gamma12 <- res[,5]
      gamma22 <- res[,6]
      
      mu0 <- input$a_mu0*exp(input$b_mu0*c(input$time[1]:input$time[2]))
      mut <- mu0 + Q01*(m1 - g01) + Q02*(m2 - g02) + Q11*(m1 - g01)^2 + 2*Q12*(m1 - g01)*(m2 - g02) + Q22*(m2 - g02)^2 + Q11*gamma11 + 2*Q12*gamma12 + Q22*gamma22
      logmut <- log(mut)
      survt <- c()
      
      t1 <- input$time[1]
      t2 <- input$time[2]
      for(i in t1:t2)
      {
          dt <- i-t1
          survt <- c(survt, exp(-1*mut[i-t1+1]*dt))
      }
      
      list(x=xx,y=yy,z=z,res=res, mut=mut, logmut=logmut, survt=survt, sigma_x=sigma_x, sigma_y=sigma_y)
      
  })
  
  PlotMain <- function(print.title=TRUE)
  {
    res <- data()
    if(print.title)
    {
        par(oma=c(1, 1, 5, 1))
        plot(res$res, cex.axis=1.5, cex.main=2, cex.lab=1.5, col="red", lwd=2)
        mtext(getPlotTitle(), side = 3, line = 0, outer = TRUE)
    }
    else 
    {
        plot(res$res, cex.axis=1.5, cex.main=2, cex.lab=1.5, col="red", lwd=2)
    }
  }
  
  PlotDensity <- function(print.title=TRUE){
    res <- data()
    persp(x=res$x, y=res$y, z=res$z, phi = 45, theta = 30, ticktype="detailed",
          xlab="x", ylab="y", zlab="z", axes=T)
    if(print.title)
        mtext(getPlotTitle(), side = 3, line = -4, outer = TRUE)
  }

  
  PlotContour <- function(print.title=TRUE){
    res <- data()
    x <- res$x
    y <- res$y
    xmin <- min(x)
    xmax <- max(x)
    ymin <- min(y)
    ymax <- max(y)
    for(i in 1:dim(res$z)[1])
    {
        if(max(res$z[i,]) >= 1e-3)
        {
            xmin <- i
            break
        }
    }
    for(i in dim(res$z)[1]:1)
    {
        if(max(res$z[i,]) >= 1e-3)
        {
            xmax <- i
            break
        }
    }
    
    for(i in 1:dim(res$z)[2])
    {
        if(max(res$z[,i]) >= 1e-3)
        {
            ymin <- i
            break
        }
    }
    for(i in dim(res$z)[2]:1)
    {
        if(max(res$z[,i]) >= 1e-3)
        {
            ymax <- i
            break
        }
    }
    
    ifelse(xmax > ymax, ymax<-xmax, xmax<-ymax)
    ifelse(xmin < ymin, ymin<-xmin, xmin<-ymin)
    
    contour(x=x[xmin:xmax], y=y[ymin:ymax], z=res$z[xmin:xmax,ymin:ymax], col="red", labcex=1, lwd=2)
    
    
    if(print.title)
        mtext(getPlotTitle(), side = 3, line = -4, outer = TRUE)
  }
  
  PlotMortSurv <- function(print.title=TRUE){
    res <- data()
    par(mfrow=c(2,1))
    plot(x=input$time[1]:input$time[2], y=res$logmut, cex.axis=1.5, cex.main=2, cex.lab=1.5, col="red", lwd=2, xlab="t", ylab="ln(mu)", type="l")
    plot(x=input$time[1]:input$time[2], y=res$surv, cex.axis=1.5, cex.main=2, cex.lab=1.5, col="blue", lwd=2, xlab="t", ylab="S", type="l")
    if(print.title)
      mtext(getPlotTitle(), side = 3, line = -4, outer = TRUE)
  }
  
  
  output$distPlot <- renderPlot({
    print(PlotMain(input$main.title))
  })
  
  output$distPlot3d <- renderPlot({
    print(PlotDensity(input$dist.title))
  })
  
  output$countourPlot <- renderPlot({
      print(PlotContour(input$contour.title))
  })
  
  output$mortSurvPlot <- renderPlot({
    print(PlotMortSurv(input$mortsurv.title))
  })
  
  output$savePlotMain <- downloadHandler(
    filename = function()
    {
      paste("plotMain_", format(Sys.time(), "%Y-%m-%dT%H_%M_%S"), ".png", sep="")
    },
    content = function(file) {
      png(file=file, width = 1024, height = 1024)
      print(PlotMain(input$main.title))
      dev.off()
    }
  ) 
  
  output$savePlotDensity <- downloadHandler(
    filename = function()
    {
      paste("plotDensity_", format(Sys.time(), "%Y-%m-%dT%H_%M_%S"), ".png", sep="")
    },
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotDensity(input$dist.title))
      dev.off()
    }
  )  
  
  output$savePlotContour <- downloadHandler(
    filename = function()
    {
      paste("plotContour_", format(Sys.time(), "%Y-%m-%dT%H_%M_%S"), ".png", sep="")
    },
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotContour(input$contour.title))
      dev.off()
    }
  ) 
  
  output$savePlotMortSurv <- downloadHandler(
    filename = function()
    {
      paste("plotMortSurv_", format(Sys.time(), "%Y-%m-%dT%H_%M_%S"), ".png", sep="")
    },
    content = function(file) {
      png(file, width = 1024, height = 1024)
      print(PlotMortSurv(input$mortsurv.title))
      dev.off()
    }
  ) 
  

  
})