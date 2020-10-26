# Copyright (C) 2019-2020 Mohammad Tarek Mansour - AFCM BioInformatics Lab
#AFCM-Egypt IGEM2020


# This file is the UI part of the platform.

# ------------------------------------------------------------------------------------

library(shiny)
library(deSolve)
library(ggplot2)
library(shinythemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$default <- renderText({ input$text })
  
  
  
  
  
  IGEM <- function(t, state, parameters) { # returns rate of change
    
    with(as.list(c(state, parameters)), {
      
      
      #
      dTC <- (K1 * Ribo * RPCC) - (K2 * TC) - (MTC * TC)
      dP <- K2 * TC - (KC * P)
      dEC <- (KC * P) - (K8 * EC) - (MEC * EC)
      dRP <- (-K3 * RP * E) + (K4 * Ribo) +KC * RP -((K7+MP) * RP)
      dRD <- K4 * Ribo +K4 * Ribo -K5 * RD * E -MD2 * RD
      dE <- (K8 * EC) + (K4 * RD) -K3 * RP * E -K5 * RD * E - MD2 * RD
      dRIP <- (K3 * RP * E) - (K4 * RP) - (MP2 * RP)
      dRID <- (K5 * RD * E) - (K9 * RD) - (MD2 * RD)
      #dRPCC <- (K2 * TC) + (K6 * RP) - (K1 * Ribo * RPCC) - (K6 * RPCC) - (ΜPC * RPCC) 
      dRPCC <- (K2*TC)+(K6*RP)-(K1*Ribo*RPCC)-(K6*RPCC)-(MPC*RPCC)
      return(list(c(dRPCC,dTC,dP,dEC,dRP,dRD,dE,dRIP,dRID)))
      
      
    })
  }
  
  
  
  
  
  
  
  
  
  
  output$guessPlot <- reactivePlot(function() {
    
    state <- c(RPCC = input$RPCi, TC = input$TCi, P = input$Pi, EC = input$ECi, RP = input$RPi, RD = input$RDi, E = input$Ei, RIP = input$RIPi, RID = input$RIDi)
    parameters <- c( K1 = input$K1, K2 = input$K2, KC = input$KC, K6 = input$K6, K7 = input$K7, K8 = input$K8, K3 = input$K3, K9 = input$K9, K4 = input$K4, K5 = input$K5, MPC = input$MPC, MP = input$MP, MD1 = input$MD1, MP2 = input$MP2, MD2 = input$MD2, MTC = input$MTC, ME = input$ME, MEC = input$MEC,Ribo = input$Ribo ) 
    time <- seq(1, 50, by = 0.01)
    #time<- seq(1,100,by=1)
    #time <- seq(1, input$tmax, by = 0.1)
    ## Integration with 'ode'
    out <- ode(y = state, times = time, func = IGEM, parms = parameters)
    
    ## Ploting
    out.df = as.data.frame(out) # required by ggplot: data object must be a data frame
    library(reshape2)
    out.m = melt(out.df, id.vars='time') # this makes plotting easier by puting all variables in a single column
    
    print(ggplot(data = out.m, aes(time, value, color = variable)) + geom_point())
    
    
  })
  
  
  
})
