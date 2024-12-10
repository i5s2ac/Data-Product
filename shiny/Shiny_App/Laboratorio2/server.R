library(shiny)
library(plotly)
library(DT)

# Definir el server
function(input, output, session) {
  
  # Mostrar la tabla de mtcars
  output$mtcarsTable <- renderDT({
    datatable(mtcars, options = list(pageLength = 5))
  })
  
  # Estado inicial del gráfico con todos los puntos
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = mtcars,
      x = ~mpg,
      y = ~wt,
      type = 'scatter',
      mode = 'markers',
      marker = list(color = 'blue', size = 10),
      source = "scatterPlot"
    )
  })
  
  # Cambiar el color en Hover
  observeEvent(event_data("plotly_hover", source = "scatterPlot"), {
    hover <- event_data("plotly_hover", source = "scatterPlot")
    if (!is.null(hover)) {
      update_plotly(session, "scatterPlot",
                    marker = list(color = ifelse(rownames(mtcars) == hover$pointNumber + 1, 'grey', 'blue'))
      )
    }
  })
  
  # Cambiar el color en Click
  observeEvent(event_data("plotly_click", source = "scatterPlot"), {
    click <- event_data("plotly_click", source = "scatterPlot")
    if (!is.null(click)) {
      update_plotly(session, "scatterPlot",
                    marker = list(color = ifelse(rownames(mtcars) == click$pointNumber + 1, 'green', 'blue'))
      )
    }
  })
  
  # Resetear el color en Doble Click
  observeEvent(event_data("plotly_doubleclick", source = "scatterPlot"), {
    update_plotly(session, "scatterPlot",
                  marker = list(color = 'blue')
    )
  })
  
  # Cambiar el color de los puntos en la selección (brush)
  observeEvent(event_data("plotly_selected", source = "scatterPlot"), {
    selected <- event_data("plotly_selected", source = "scatterPlot")
    if (!is.null(selected)) {
      selected_points <- selected$pointNumber + 1
      update_plotly(session, "scatterPlot",
                    marker = list(color = ifelse(rownames(mtcars) %in% selected_points, 'red', 'blue'))
      )
    }
  })
}

# Función auxiliar para actualizar el gráfico
update_plotly <- function(session, plot_id, marker) {
  plotlyProxy(plot_id, session) %>%
    plotlyProxyInvoke("restyle", list(marker = marker))
}

