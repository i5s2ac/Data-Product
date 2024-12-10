library(shiny)

fluidPage(
  
  # Application title
  titlePanel("UI dinamico, UpdateInput"),
  
  tabsetPanel(
    tabPanel('Ejemplo 1',
             numericInput('limiteInferior', 
                          label = 'Ingrese Limite Inferior',
                          value = 1),
             
             numericInput('limiteSuperior', 
                          label = 'Ingrese Limite Superior',
                          value = 1),
             
             sliderInput('sl1',
                         label = 'Seleccione intervalo',
                         value = 5,
                         min= 0, max = 15)
    )
  )
)
