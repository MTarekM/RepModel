# Copyright (C) 2019-2020 Mohammad Tarek Mansour - AFCM BioInformatics Lab
#AFCM-Egypt IGEM2020


# This file is the UI part of the platform.

# ------------------------------------------------------------------------------------

library(shiny)
library(deSolve)
library(ggplot2)
library(shinythemes)

# Define UI for application that draws a histogram
ui<- shinyUI(fluidPage(theme = shinytheme("paper"),
                       
                       list(tags$head(HTML('<link rel="icon", href="https://raw.githubusercontent.com/MTarekM/AFCMHeatMapGeneratingTool/master/afcm.png", 
                                           
                                           type="image/png" />'))),
                       
                       div(style="padding: 1px 0px; width: '100%'",
                           
                           titlePanel(
                             
                             title="", windowTitle="RepModel: Modeling Self-Amplifying Replicons"
                             
                           )
                           
                       ),
                       pageWithSidebar(
                         
                         
                         
                         # Application title
                         
                         
                         
                         headerPanel(h1(a(img(src = "https://docs.google.com/drawings/d/e/2PACX-1vQISqS-T7AZsF_TOrGreroMyA11MlKvizBxL_7CEYLM8q56_BRxDDXy7imBUxfAt-9ND740nyuurmlV/pub?w=960&h=720", align = "center", width = "100%",height="250px"), href = "https://2020.igem.org/Team:AFCM-Egypt"))),
                         
                         
                         
                         # Sidebar with controls
                         
                         sidebarPanel(
                           
                           
                           
                           helpText(a("AFCM RepModel tool Source Code on GitHub", href = "https://github.com/MTarekM/RepModel", target = "_blank")),
                           sliderInput("RPCi", "RPCC: Initial  Plus-strand RNA Concentration",  min = 0,max = 20, value = 2, step=1),
                           sliderInput("TCi", "TCi: Initial translation complexes Concentration", min = 0,max = 20, value = 0, step=1),
                           sliderInput("Pi", "Pi: Initial Free viral polyprotein molecules Concentration", min = 0,max = 20, value = 0, step=1),
                           sliderInput("ECi", "ECi: Initial and the enzyme NS5B and associated viral proteins needed for RNA synthesis Concentration", min = 0,max = 20, value = 0, step=1),
                           sliderInput("RPi", "RPi: Initial the numbers of plus-strand RNA within the VMS ( after self-amplification) Concentration", min = 0,max = 20, value = 1, step=1),
                           sliderInput("RDi", "RDi: Initial dsRNA within the VMS Concentration", min = 0,max = 20, value = 1, step=1),
                           sliderInput("Ei", "Ei: Initial viral polymerase complexes within the VMS Concentration", min = 0,max = 20, value = 1, step=1),
                           sliderInput("RIPi", "RIPi: Initial   the numbers of plus-strand RNA replicative intermediate complexes Concentration", min = 0,max = 20, value = 1, step=1),
                           sliderInput("RIDi", "RIDi: Initial  the numbers of dsRNA replicative intermediate complexes Concentration", min = 0,max = 20, value = 1, step=1),
                           sliderInput("Ribo", "Ribo: Initial  the numbers of Ribosomes", min = 0,max = 20, value = 10, step=1),
                           
                           
                           sliderInput("K1", "Rate of Tc formation", min = 0,max = 5, value = 1.155, step=.1),  
                           sliderInput("K2", "Rate of Nascent NS polyprotein translation", min = 0,max = 400, value = 100, step=.1), 
                           sliderInput("KC", "Rate of Viral polyprotein cleavage dissociation", min = 0,max = 5, value = 0.2, step=.1),    
                           sliderInput("K6", "Rate of RPC transport into VMS ", min = 0,max = 5, value = 0.2, step=.1),
                           sliderInput("K7", "Rate of RP transport into cytoplasm ", min = 0,max = 5, value = 0.2, step=.1),
                           sliderInput("K8", "Rate of EC  transport into VMS", min = 0,max = 5, value = 0.000004, step=.1),
                           sliderInput("K3", "Rate of RIP formation", min = 0,max = 5, value = 0.001, step=.1),
                           sliderInput("K9", "Rate of RP synthesis", min = 0,max = 5, value = 1.7, step=.1),
                           sliderInput("K4", "Rate of RD synthesis", min = 0,max = 5, value = 1.7, step=.1),
                           sliderInput("K5", "Rate of RID formation", min = 0,max = 500, value = 200, step=.1),
                           sliderInput("MPC", "Rate of RPC degradation", min = 0,max = 5, value = 0.06, step=.1),
                           sliderInput("MP", "Rate of RP degradation", min = 0,max = 5, value = 0.07, step=.1),
                           sliderInput("MD1", "Rate of RD  degradation", min = 0,max = 5, value = 0.06, step=.1),
                           sliderInput("MP2", "Rate of RIP degradation", min = 0,max = 5, value = 0.01, step=.1),
                           sliderInput("MD2", "Rate of RID degradation", min = 0,max = 400, value = 104, step=.1),
                           sliderInput("MTC", "Rate of TC degradation", min = 0,max = 5, value = 0.001, step=.1),
                           sliderInput("ME", "Rate of E degradation", min = 0,max = 5, value = 0.001, step=.1),
                           sliderInput("MEC", "Rate of EC degradation ", min = 0,max = 5, value = 0.06, step=.1)
                           
                           
                         ),
                         
                         
                         
                         
                         
                         # Show the plot of the requested variable against mpg
                         
                         mainPanel("AFCM-RepModel",
                                   
                                   textInput("text", "AFCMEasy Model was a tool developed as a part of Team AFCM-Egypt 2017 IGEM project. It is an interactive web tool that embeds R Codes used for modeling ceRNA networks to solve ODEs easily, specify parameters, rates and reaction species. The tool aimed to help other teams model their projects in an easy interactive way without facing technical issues. And for this year, Team AFCM-EGYPT 2020 decided to construct an efficient tool through improving and developing our previous made tool. It’s done via modification of the tool source code provided in Github to include the equations and parameters simulating replicon amplification which is the core. Thereby being beneficial to us and other teams who will depend on replicon in their projects to consume the time needed for the calculations and the simulation as well. In addition to consuming less time , You may also save your plot as an image and modify the parameter values within a defined range to be suitable for each project.The tool depends on a set of differential equations inspired by previous literature and further developed by our team. You could modify parameters to be fitting your project. You may also save your plot as an image.",width = 900),
                                   
                                   
                                   plotOutput("guessPlot",height = 800))
                         
                         
                         
                       ),
                       tags$footer("Developed at Bioinformatics & Computational Biology Lab - AFCM 2020 under the
                                   
                                   GNU General Public License v3.0", align = "center", style = "
                                   
                                   position:bottom;
                                   
                                   bottom:0;
                                   
                                   width:100%;
                                   
                                   height:25px;   /* Height of the footer */
                                   
                                   color: black;
                                   
                                   padding: 10px;
                                   
                                   background-color: white;
                                   
                                   z-index: 1000;
                                   align: center;")
                       
                       )
             
                       )
