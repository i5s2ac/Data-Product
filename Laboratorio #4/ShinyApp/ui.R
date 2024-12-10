# ui.R

library(shiny)
library(plotly)
library(leaflet)
library(DT)

shinyUI(
  fluidPage(
    titlePanel("Consulta de Clima - Selección Múltiple con Indicadores"),
    sidebarLayout(
      sidebarPanel(
        selectInput("unidades_coord", "Unidades:",
                    choices = c("Métrico" = "metric", "Imperial" = "imperial")),
        actionButton("limpiar_puntos", "Limpiar Puntos Seleccionados"),
        downloadButton("descargar_datos", "Descargar Datos"),
        br(), br(),
        # Usar selectizeInput para mejor manejo
        selectizeInput(
          inputId = "combo_ciudad",
          label = "Seleccionar Ciudad(es):",
          choices = NULL,
          multiple = TRUE,
          options = list(
            plugins = list('remove_button'),
            placeholder = 'Selecciona una o más ciudades'
          )
        ),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Mapa Interactivo y Tabla",
            fluidRow(
              column(12, leafletOutput("mapa", height = "400px")),
              column(12, DTOutput("tabla_puntos", height = "300px"))
            )
          ),
          tabPanel(
            "Indicadores",
            # Contenedor dinámico para múltiples indicadores con títulos
            uiOutput("indicadores_ui")
          ),
          tabPanel(
            "Pronóstico a 5 Días",
            fluidRow(
              # Contenedor dinámico para gráficos y tablas de pronóstico
              column(12, uiOutput("grafico_pronostico")),
              column(12, uiOutput("tabla_pronostico"))
            )
          ),
         
        ),
        width = 9
      )
    )
  )
)
