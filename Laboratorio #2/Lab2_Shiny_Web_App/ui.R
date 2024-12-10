library(shiny)

# Define UI for the application
fluidPage(
  titlePanel("Lab 2. Shiny Plot"),
  tags$h4("By Isaac Cyrman (20220289)"),
  
  
  # Gráfico interactivo con diferentes interacciones
  plotOutput('interactive_plot',
             click = 'plot_click',
             dblclick = 'plot_dblclick',
             hover = 'plot_hover',
             brush = 'plot_brush'),
  
  # Mostrando información de los clics y selección
  h3("Información de Clics y Selección"),
  uiOutput("click_info"),
  uiOutput("brush_info")
)
