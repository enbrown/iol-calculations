library(shiny)
library(IOL)

shinyServer(function(input, output) {
  
  output$eyePlot <- renderPlot({
    op <- par(mfrow=c(2,2))
    R1 <- 331.48 / input$K
    R2 <- 337.5 / input$K
    
    cortex1 = 10.0 * input$lens_scale
    nucleus1 = 7.911 * input$lens_scale
    nucleus2 = -5.76 * input$lens_scale
    cortex2 = -6 * input$lens_scale
    
    IOL:::eye(AL = input$L, cornea1 = R2,
              cortex1 = cortex1, nucleus1 = nucleus1, nucleus2 = nucleus2, cortex2 = cortex2,
              Rx = input$Rx)
    IOL:::eye(AL = input$L, cornea1 = R2,
              cortex1 = cortex1, nucleus1 = nucleus1, nucleus2 = nucleus2, cortex2 = cortex2,
              Rx = input$Rx, zoom = 10)
    IOL:::eye(AL = input$L, cornea1 = R2, Rx = input$Rx2,
              PCIOL = TRUE, D = input$P, ELP = input$ELP)
    IOL:::eye(AL = input$L, cornea1 = R2, Rx = input$Rx2,
              PCIOL = TRUE, D = input$P, ELP = input$ELP,
              zoom = 10)
    par(op)
  })
  
  ELP <- reactive({
    elp <- IOL::ELP(L = input$L, K = input$K, A = input$A, which = 'Hoffer.Q')
    return(elp)
  })
  
  Power <- reactive({
    power <- IOL::Power(L = input$L, K = input$K, ELP = input$ELP, which = 'Hoffer.Q')
    return(power)
  })
  
  output$ELP <- renderText({
    paste0("Hoffer-Q ELP = ", IOL:::Hoffer.Q.ELP(L = input$L, K = input$K, A = input$A))
  })
  
  output$Power <- renderText({
    paste0("Hoffer-Q Power = ", IOL:::Hoffer.Q.Power(L = input$L, K = input$K, ELP = input$ELP))
  })
})
