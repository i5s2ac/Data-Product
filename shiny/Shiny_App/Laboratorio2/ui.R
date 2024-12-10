library(shiny)
library(plotly)
library(DT)

# Definir el UI
fluidPage(
  titlePanel("Interactive mtcars plot"),
  sidebarLayout(
    sidebarPanel(
      h4("Gráfico interactivo usando Shiny y Plotly"),
      width = 3
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
      DTOutput("mtcarsTable"),
      width = 9
    )
  )
)
