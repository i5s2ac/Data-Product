library(shiny)
library(lubridate)

colores = c("azul", "verde","amarillo","rojo")
carros= row.names(mtcars)

# Define UI for application that draws a histogram
fluidPage(

    titlePanel("Catalogo de inputs"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          ### inputs
          sliderInput("sliderInputSingle", 
                      "Seleccione un número",
                      # Post = "%",
                      pre = "Q. ",
                      min=0,
                      max=100,
                      value=50,
                      step=10,
                      animate= animationOptions(
                      interval =500,
                      loop=TRUE,
                      playButton="Avanza",
                      pauseButton="Detente")),
          sliderInput("SliderInputMulti", "seleccione un rango:",
                      min = -10, max=10, value = c(0,5)),
          numericInput("numericInput1","Seleccione: " ,value=10, step=1),
          dateInput("dateInputSingle","Seleccione una fecha: ",language = "es",weekstart = 1,daysofweekdisabled = c(0,6)),
          dateRangeInput("dateRange", "Seleccione fechas: ",separator = "hasta", language = "es", 
                         start = today()-7, end = today(),
                         min = today() - 30, max= today()),
          selectInput("selectInputSingle","Seleccione: ", 
                      choices = colores, 
                      selected = colores[1]),
          selectInput("SelectInputMulti","Seleccione: ", 
                      choices =carros, 
                      selected = carros[sample(1:length(carros),3)], 
                      multiple = TRUE )
         
        ),

        # Show a plot of the generated distribution
        mainPanel(
          ### outputs
          h2("slider input"),
          verbatimTextOutput("sis"),
          h2("slider input multiple"),
          verbatimTextOutput("siMulti"),
          h2("Numeric Input"),
          verbatimTextOutput("NumericIO"),
          h2("SingleDate Input IO"),
          verbatimTextOutput("DateSingle"),
          h2("Date Range IO"),
          verbatimTextOutput("DateRange"),
          h2("Single Select Input"),
          verbatimTextOutput("SingleSelectedInput"),
          h2("Multi Select Input"),
          verbatimTextOutput("MultiSelectedInput"),
          
          
          
        
          
          
        )
    )
)
