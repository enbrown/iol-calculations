library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel("IOL Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("With Natural Lens"),
      numericInput("Rx", 
                   label = "Glasses Prescription", 
                   value = -3.375),
      numericInput("L", 
                   label = "Axial Length", 
                   value = 25.47),
      numericInput("K", 
                   label = "Keratometry", 
                   value = 40.935),
      numericInput("lens_scale", 
                   label = "Lens Scale Factor", 
                   value = 0.84),
      hr(),
      h3("With IOL"),
      numericInput("Rx2",
                   label = "Glasses Prescription", 
                   value = 0.0),
      numericInput("A",
                   label = 'IOL Lens A Constant',
                   value = 119.2),
      numericInput("ELP",
                   label = "Effective Lens Position", 
                   value = 6.01),
      textOutput("ELP"),
      numericInput("P",
                   label = "IOL Power", 
                   value = 18.56),
      textOutput("Power")
    ),
    
    mainPanel(
      plotOutput("eyePlot", height = '600px')
    )
  )
))
