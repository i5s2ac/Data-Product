library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$sis <- renderPrint({
    out <- input$sliderInputSingle
    print(out)
    
  })
  
  output$siMulti <- renderPrint({
    print(input$SliderInputMulti)

  })
  
  output$NumericIO <- renderPrint({
    print(input$numericInput1)
    
  })
  
  output$DateSingle <- renderPrint({
    print(input$dateInputSingle)
    
  })
  
  output$DateRange <- renderPrint({
    print(input$dateRange)
    
  })
  
  output$SingleSelectedInput <- renderPrint({
    print(input$selectInputSingle)
    
  })
  
  output$MultiSelectedInput <- renderPrint({
    print(input$SelectInputMulti)
    
  })
  
}

