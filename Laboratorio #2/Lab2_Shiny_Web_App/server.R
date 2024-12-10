library(shiny)
library(DT)

# Define server logic
function(input, output, session) {
  
  # Valores reactivos para almacenar el estado de los colores y las interacciones
  rv <- reactiveValues(
    point_colors = rep("black", nrow(mtcars)),  # Color por defecto
    hover_point = NULL,
    clicked_points = NULL,
    brushed_points = NULL
  )
  
  # Renderizar el gráfico interactivo
  output$interactive_plot <- renderPlot({
    plot(
      mtcars$wt, mtcars$mpg,
      xlab = 'Peso (wt)',
      ylab = 'Millas por Galón (mpg)',
      col = rv$point_colors,  # Colores dinámicos
      pch = 16,  # Puntos sólidos
      cex = 1.8  # Ajustar tamaño de los puntos
    )
    
    # Usar mtext() en lugar de title() para alinear el título a la izquierda
    mtext("Gráfico Interactivo de MTCARS", side = 3, line = 1, adj = 0, cex = 1.5, font = 2)
  })
  
  
  # Lógica para el hover
  observeEvent(input$plot_hover, {
    hover_info <- input$plot_hover
    hover_point <- nearPoints(
      mtcars, 
      hover_info,
      xvar = "wt", 
      yvar = "mpg",
      threshold = 10,
      maxpoints = 1
    )
    
    if (nrow(hover_point) > 0) {
      rv$hover_point <- rownames(hover_point)
    } else {
      rv$hover_point <- NULL
    }
    
    # Actualización de colores
    rv$point_colors <- ifelse(
      rownames(mtcars) %in% rv$hover_point, "gray",  # Hover gris
      ifelse(
        rownames(mtcars) %in% rv$clicked_points, "green",  # Clic verde
        ifelse(
          rownames(mtcars) %in% rv$brushed_points, "green",  # Selección azul
          "black"  # Color por defecto
        )
      )
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Lógica para los clics
  observeEvent(input$plot_click, {
    clicked_point <- nearPoints(
      mtcars, 
      input$plot_click,
      xvar = "wt", 
      yvar = "mpg"
    )
    
    if (nrow(clicked_point) > 0) {
      rv$clicked_points <- unique(c(rv$clicked_points, rownames(clicked_point)))
    }
    
    # Actualizar colores después del clic
    rv$point_colors <- ifelse(
      rownames(mtcars) %in% rv$hover_point, "gray",
      ifelse(
        rownames(mtcars) %in% rv$clicked_points, "green",
        ifelse(
          rownames(mtcars) %in% rv$brushed_points, "green",
          "black"
        )
      )
    )
    
    # Mostrar los datos del punto clicado en formato de tabla
    output$click_info <- renderUI({
      clicked_data <- mtcars[rownames(mtcars) %in% rv$clicked_points, ]
      if (nrow(clicked_data) > 0) {
        tagList(
          h4("Datos de los puntos clicados:"),
          tableOutput("click_table")
        )
      } else {
        NULL
      }
    })
    
    output$click_table <- renderTable({
      mtcars[rownames(mtcars) %in% rv$clicked_points, ]
    }, striped = TRUE, hover = TRUE)
  })
  
  # Lógica para el doble clic (quitar el punto clicado)
  observeEvent(input$plot_dblclick, {
    dbl_clicked_point <- nearPoints(
      mtcars, 
      input$plot_dblclick,
      xvar = "wt", 
      yvar = "mpg"
    )
    
    if (nrow(dbl_clicked_point) > 0) {
      rv$clicked_points <- setdiff(rv$clicked_points, rownames(dbl_clicked_point))
      rv$brushed_points <- setdiff(rv$brushed_points, rownames(dbl_clicked_point))
    }
    
    # Actualizar los colores tras el doble clic
    rv$point_colors <- ifelse(
      rownames(mtcars) %in% rv$hover_point, "gray",
      ifelse(
        rownames(mtcars) %in% rv$clicked_points, "green",
        ifelse(
          rownames(mtcars) %in% rv$brushed_points, "green",
          "black"
        )
      )
    )
  })
  
  # Lógica para la selección de puntos (brush)
  observeEvent(input$plot_brush, {
    brushed_points <- brushedPoints(
      mtcars, 
      input$plot_brush,
      xvar = "wt", 
      yvar = "mpg"
    )
    
    if (nrow(brushed_points) > 0) {
      rv$brushed_points <- rownames(brushed_points)
    } else {
      rv$brushed_points <- NULL
    }
    
    # Actualizar colores para la selección de puntos
    rv$point_colors <- ifelse(
      rownames(mtcars) %in% rv$hover_point, "gray",
      ifelse(
        rownames(mtcars) %in% rv$clicked_points, "green",
        ifelse(
          rownames(mtcars) %in% rv$brushed_points, "green",
          "black"
        )
      )
    )
    
    # Mostrar los datos de los puntos seleccionados en formato de tabla
    output$brush_info <- renderUI({
      if (!is.null(rv$brushed_points)) {
        tagList(
          h4("Datos de los puntos seleccionados:"),
          tableOutput("brush_table")
        )
      }
    })
    
    output$brush_table <- renderTable({
      mtcars[rownames(mtcars) %in% rv$brushed_points, ]
    }, striped = TRUE, hover = TRUE)
  })
}

