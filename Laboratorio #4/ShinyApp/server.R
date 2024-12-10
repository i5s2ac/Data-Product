# server.R

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)  # Para funciones de manipulación de datos

# URL base de la API
api_url <- "http://localhost:8001"

shinyServer(function(input, output, session) {
  # ReactiveVal para almacenar los puntos seleccionados
  puntos_seleccionados <- reactiveVal(data.frame(
    Ciudad = character(),
    Temperatura = numeric(),
    Humedad = numeric(),
    Viento = character(),
    Latitud = numeric(),
    Longitud = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Función para manejar valores NULL
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Función para sanitizar nombres de ciudades para usarlos como IDs
  sanitize_id <- function(name) {
    sanitized <- gsub(" ", "_", name)  # Reemplazar espacios por guiones bajos
    sanitized <- gsub("[^A-Za-z0-9_]", "", sanitized)  # Eliminar caracteres no permitidos
    sanitized <- paste0("ciudad_", sanitized)  # Añadir un prefijo para evitar IDs vacíos
    return(sanitized)
  }
  
  # Función para obtener clima por coordenadas
  obtener_clima_coordenadas <- function(lat, lon, unidades) {
    query_params <- list(lat = lat, lon = lon, unidades = unidades)
    response <- GET(url = paste0(api_url, "/clima_coordenadas"), query = query_params)
    if (status_code(response) == 200) {
      content(response, as = "parsed", simplifyVector = TRUE)
    } else {
      NULL
    }
  }
  
  # Función para obtener pronóstico por ciudad
  obtener_pronostico <- function(ciudad, unidades) {
    query_params <- list(ciudad = ciudad, unidades = unidades)
    response <- GET(url = paste0(api_url, "/pronostico"), query = query_params)
    if (status_code(response) == 200) {
      content(response, as = "parsed", simplifyVector = TRUE)
    } else {
      NULL
    }
  }
  
  # Definir la función ciudad_pronostico
  ciudad_pronostico <- function(ciudades) {
    req(ciudades)  # Asegura que 'ciudades' no sea NULL
    
    unidades <- input$unidades_coord  # Obtener las unidades seleccionadas
    
    # Generar pronóstico para cada ciudad
    pronosticos <- lapply(ciudades, function(ciudad) {
      data <- obtener_pronostico(ciudad, unidades)
      
      if (!is.null(data) && !is.null(data$pronostico)) {
        fechas <- as.Date(sapply(data$pronostico$fecha_hora, function(x) substr(x, 1, 10)))
        temperaturas <- sapply(data$pronostico$temperatura, as.numeric)
        humedades <- sapply(data$pronostico$humedad, as.numeric)
        
        pronostico_df <- data.frame(
          Fecha = fechas,
          Temperatura = temperaturas,
          Humedad = humedades,
          stringsAsFactors = FALSE
        )
        
        promedios_diarios <- aggregate(cbind(Temperatura, Humedad) ~ Fecha,
                                       data = pronostico_df, FUN = mean)
        
        list(ciudad = ciudad, data = promedios_diarios)
      } else {
        list(ciudad = ciudad, data = NULL)
      }
    })
    
    # Renderizar tablas de pronóstico para cada ciudad
    output$tabla_pronostico <- renderUI({
      tablas <- lapply(pronosticos, function(pronostico) {
        if (is.null(pronostico$data)) {
          box(title = paste("Pronóstico para", pronostico$ciudad),
              status = "danger",
              solidHeader = TRUE,
              "No hay datos disponibles.")
        } else {
          fluidRow(
            column(12, h3(paste("Pronóstico para", pronostico$ciudad))),
            column(12, DT::datatable(pronostico$data, options = list(pageLength = 5)))
          )
        }
      })
      do.call(tagList, tablas)
    })
    
    # Renderizar gráficos de pronóstico para cada ciudad
    output$grafico_pronostico <- renderUI({
      graficos <- lapply(pronosticos, function(pronostico) {
        if (is.null(pronostico$data)) {
          NULL
        } else {
          sanitized_ciudad <- sanitize_id(pronostico$ciudad)
          plotlyOutput(paste0("grafico_pronostico_", sanitized_ciudad), height = "400px")
        }
      })
      do.call(tagList, graficos)
    })
    
    # Generar gráficos para cada pronóstico
    lapply(pronosticos, function(pronostico) {
      if (!is.null(pronostico$data)) {
        sanitized_ciudad <- sanitize_id(pronostico$ciudad)
        output[[paste0("grafico_pronostico_", sanitized_ciudad)]] <- renderPlotly({
          tryCatch({
            plot_ly(data = pronostico$data,
                    x = ~Fecha,
                    y = ~Temperatura,
                    type = 'scatter',
                    mode = 'lines+markers',
                    name = "Temperatura",
                    marker = list(color = 'blue')) %>%
              add_trace(y = ~Humedad, name = "Humedad", marker = list(color = 'lightblue')) %>%
              layout(
                title = paste("Pronóstico de Temperatura y Humedad para", pronostico$ciudad),
                xaxis = list(title = "Fecha", tickformat = "%d-%m"),
                yaxis = list(title = "Valor")
              )
          }, error = function(e) {
            print(paste("Error en grafico_pronostico:", e$message))
            NULL
          })
        })
      }
    })
  }
  
  # Renderizar el mapa inicial
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -58.3816, lat = -34.6037, zoom = 4)  # Vista inicial en Buenos Aires
  })
  
  # Sincronizar el Combo Box cada vez que se actualicen los puntos seleccionados
  observe({
    puntos <- puntos_seleccionados()
    # Eliminar duplicados para evitar múltiples entradas de la misma ciudad
    unique_ciudades <- unique(puntos$Ciudad)
    
    if (length(unique_ciudades) > 0) {
      updateSelectInput(session, "combo_ciudad", choices = unique_ciudades, selected = unique_ciudades)
    } else {
      updateSelectInput(session, "combo_ciudad", choices = unique_ciudades, selected = NULL)
    }
  })
  
  # Observador para detectar cambios en el selectInput y actualizar puntos_seleccionados
  observeEvent(input$combo_ciudad, {
    # Obtener las ciudades actualmente seleccionadas en el selectInput
    selected_ciudades <- input$combo_ciudad
    
    # Obtener todas las ciudades actualmente en puntos_seleccionados
    current_ciudades <- puntos_seleccionados()$Ciudad
    
    # Identificar las ciudades que se han eliminado
    ciudades_eliminadas <- setdiff(current_ciudades, selected_ciudades)
    
    # Eliminar las ciudades deseleccionadas de puntos_seleccionados
    if (length(ciudades_eliminadas) > 0) {
      nuevos_puntos <- puntos_seleccionados() %>% 
        filter(!(Ciudad %in% ciudades_eliminadas))
      
      puntos_seleccionados(nuevos_puntos)
      
      # Eliminar los marcadores correspondientes del mapa
      lapply(ciudades_eliminadas, function(ciudad) {
        sanitized_ciudad <- sanitize_id(ciudad)
        leafletProxy("mapa") %>% removeMarker(layerId = sanitized_ciudad)
      })
      
      # Limpiar los indicadores y pronósticos asociados a las ciudades eliminadas
      output$indicadores_ui <- renderUI({
        req(input$combo_ciudad)
        ciudades <- input$combo_ciudad
        indicadores <- lapply(ciudades, function(ciudad) {
          punto <- puntos_seleccionados() %>% filter(Ciudad == ciudad)
          if (nrow(punto) == 0) return(NULL)
          
          sanitized_ciudad <- sanitize_id(ciudad)
          
          fluidRow(
            column(12, h4(paste("Indicadores para", ciudad))),  # Título para cada ciudad
            column(6, plotlyOutput(paste0("indicador_temp_", sanitized_ciudad), height = "300px")),
            column(6, plotlyOutput(paste0("indicador_humedad_", sanitized_ciudad), height = "300px"))
          )
        })
        do.call(tagList, indicadores)
      })
      
      # Actualizar pronósticos para las ciudades seleccionadas
      ciudad_pronostico(selected_ciudades)
    }
    
    # Nota: En este caso, no manejamos ciudades agregadas desde el selectInput,
    # ya que las ciudades se agregan solo desde el mapa. Si existiera otra forma,
    # deberías manejar ciudades_agregadas similar a ciudades_eliminadas.
    
  }, ignoreInit = TRUE)
  
  # Observador para manejar la selección en la tabla y actualizar el selectInput
  observeEvent(input$tabla_puntos_rows_selected, {
    selected_rows <- input$tabla_puntos_rows_selected
    puntos <- puntos_seleccionados()
    
    if (length(selected_rows) == 0) {
      # Limpiar todas las selecciones
      puntos_seleccionados(data.frame(
        Ciudad = character(),
        Temperatura = numeric(),
        Humedad = numeric(),
        Viento = character(),
        Latitud = numeric(),
        Longitud = numeric(),
        stringsAsFactors = FALSE
      ))
      updateSelectInput(session, "combo_ciudad", selected = NULL)
      leafletProxy("mapa") %>% clearMarkers()
      
      # Limpiar indicadores y pronósticos
      output$indicadores_ui <- renderUI(NULL)
      output$grafico_pronostico <- renderUI(NULL)
      output$tabla_pronostico <- renderUI(NULL)
    } else {
      # Filtrar los puntos seleccionados
      puntos_seleccionados_seleccionados <- puntos[selected_rows, ]
      selected_ciudades <- puntos_seleccionados_seleccionados$Ciudad
      
      # Actualizar el selectInput para reflejar las selecciones
      updateSelectInput(session, "combo_ciudad", selected = selected_ciudades)
      
      # Enfocar el mapa para incluir todas las ciudades seleccionadas
      leafletProxy("mapa") %>%
        fitBounds(
          lng1 = min(puntos_seleccionados_seleccionados$Longitud),
          lat1 = min(puntos_seleccionados_seleccionados$Latitud),
          lng2 = max(puntos_seleccionados_seleccionados$Longitud),
          lat2 = max(puntos_seleccionados_seleccionados$Latitud)
        )
      
      # Generar pronósticos para las ciudades seleccionadas
      ciudad_pronostico(selected_ciudades)
    }
  })
  
  # Observador adicional para detectar cuando todas las ciudades están deseleccionadas en el selectInput
  observe({
    if (is.null(input$combo_ciudad) || length(input$combo_ciudad) == 0) {
      # Limpiar todos los puntos seleccionados
      puntos_seleccionados(data.frame(
        Ciudad = character(),
        Temperatura = numeric(),
        Humedad = numeric(),
        Viento = character(),
        Latitud = numeric(),
        Longitud = numeric(),
        stringsAsFactors = FALSE
      ))
      
      # Limpiar el mapa
      leafletProxy("mapa") %>% clearMarkers()
      
      # Limpiar indicadores y pronósticos
      output$indicadores_ui <- renderUI(NULL)
      output$grafico_pronostico <- renderUI(NULL)
      output$tabla_pronostico <- renderUI(NULL)
    }
  })
  
  # Manejar los clics en el mapa para agregar puntos
  observeEvent(input$mapa_click, {
    coordenadas <- input$mapa_click
    lat <- coordenadas$lat
    lon <- coordenadas$lng
    
    data <- obtener_clima_coordenadas(lat, lon, input$unidades_coord)
    
    if (!is.null(data)) {
      nueva_ciudad <- data$ciudad %||% "No disponible"
      puntos <- puntos_seleccionados()
      
      # Verificar si la ciudad ya está seleccionada
      if (!(nueva_ciudad %in% puntos$Ciudad)) {
        nuevo_punto <- data.frame(
          Ciudad = nueva_ciudad,
          Temperatura = data$temperatura %||% NA,
          Humedad = data$humedad %||% NA,
          Viento = paste0(data$viento$velocidad, " m/s, Dirección: ", data$viento$direccion),
          Latitud = lat,
          Longitud = lon,
          stringsAsFactors = FALSE
        )
        
        puntos_actualizados <- bind_rows(puntos, nuevo_punto)
        puntos_seleccionados(puntos_actualizados)
        
        # Sanitizar el nombre de la ciudad para usar como ID
        sanitized_ciudad <- sanitize_id(nueva_ciudad)
        
        # Actualizar el selectInput para incluir la nueva ciudad
        updateSelectInput(session, "combo_ciudad", choices = unique(puntos_actualizados$Ciudad), selected = unique(puntos_actualizados$Ciudad))
        
        # Agregar marcador al mapa con ID único
        leafletProxy("mapa") %>%
          addMarkers(
            lng = lon,
            lat = lat,
            layerId = sanitized_ciudad,  # Asignar un ID único al marcador
            popup = paste0("<b>Ciudad:</b> ", nuevo_punto$Ciudad,
                           "<br><b>Temperatura:</b> ", nuevo_punto$Temperatura, "°C")
          )
        
        # Generar pronósticos para las ciudades seleccionadas
        ciudad_pronostico(unique(puntos_actualizados$Ciudad))
      }
    }
  })
  
  # Renderizar la tabla de puntos seleccionados
  output$tabla_puntos <- renderDT({
    datatable(puntos_seleccionados(), selection = "multiple", options = list(pageLength = 5))
  })
  
  # Generar indicadores dinámicamente para cada ciudad seleccionada
  output$indicadores_ui <- renderUI({
    req(input$combo_ciudad)
    ciudades <- input$combo_ciudad
    indicadores <- lapply(ciudades, function(ciudad) {
      punto <- puntos_seleccionados() %>% filter(Ciudad == ciudad)
      if (nrow(punto) == 0) return(NULL)
      
      sanitized_ciudad <- sanitize_id(ciudad)
      
      fluidRow(
        column(12, h4(paste("Indicadores para", ciudad))),  # Título para cada ciudad
        column(6, plotlyOutput(paste0("indicador_temp_", sanitized_ciudad), height = "300px")),
        column(6, plotlyOutput(paste0("indicador_humedad_", sanitized_ciudad), height = "300px"))
      )
    })
    do.call(tagList, indicadores)
  })
  
  # Renderizar indicadores para cada ciudad seleccionada
  observe({
    req(input$combo_ciudad)
    ciudades <- input$combo_ciudad
    lapply(ciudades, function(ciudad) {
      punto <- puntos_seleccionados() %>% filter(Ciudad == ciudad)
      if (nrow(punto) == 0) return(NULL)
      
      sanitized_ciudad <- sanitize_id(ciudad)
      
      # Renderizar indicador de Temperatura
      output[[paste0("indicador_temp_", sanitized_ciudad)]] <- renderPlotly({
        tryCatch({
          plot_ly(
            type = "indicator",
            mode = "gauge+number",
            value = punto$Temperatura,
            title = list(text = paste("Temperatura en", ciudad)),
            gauge = list(
              axis = list(range = list(-30, 50)),
              bar = list(color = "blue")
            )
          )
        }, error = function(e) {
          print(paste("Error en indicador_temp:", e$message))
          NULL  # Retorna NULL para evitar que Shiny falle
        })
      })
      
      # Renderizar indicador de Humedad
      output[[paste0("indicador_humedad_", sanitized_ciudad)]] <- renderPlotly({
        tryCatch({
          plot_ly(
            type = "indicator",
            mode = "gauge+number",
            value = punto$Humedad,
            title = list(text = paste("Humedad en", ciudad)),
            gauge = list(
              axis = list(range = list(0, 100)),
              bar = list(color = "lightblue")
            )
          )
        }, error = function(e) {
          print(paste("Error en indicador_humedad:", e$message))
          NULL  # Retorna NULL para evitar que Shiny falle
        })
      })
    })
  })
  
  # Manejar la selección desde el Combo Box para sincronizar con el mapa y la tabla
  observeEvent(input$combo_ciudad, {
    req(input$combo_ciudad)
    
    # Encontrar las filas correspondientes a las ciudades seleccionadas
    selected_rows <- which(puntos_seleccionados()$Ciudad %in% input$combo_ciudad)
    if (length(selected_rows) == 0) return()
    
    # Seleccionar las filas en la tabla
    proxy <- dataTableProxy("tabla_puntos")
    selectRows(proxy, selected_rows)
    
    # Enfocar el mapa para incluir todas las ciudades seleccionadas
    puntos <- puntos_seleccionados()[selected_rows, ]
    leafletProxy("mapa") %>%
      fitBounds(
        lng1 = min(puntos$Longitud),
        lat1 = min(puntos$Latitud),
        lng2 = max(puntos$Longitud),
        lat2 = max(puntos$Latitud)
      )
  })
  
  # Manejar los pronósticos para múltiples ciudades
  observeEvent(input$combo_ciudad, {
    req(input$combo_ciudad)
    
    ciudades <- input$combo_ciudad
    unidades <- input$unidades_coord
    
    # Generar pronóstico para cada ciudad
    pronosticos <- lapply(ciudades, function(ciudad) {
      data <- obtener_pronostico(ciudad, unidades)
      
      if (!is.null(data) && !is.null(data$pronostico)) {
        fechas <- as.Date(sapply(data$pronostico$fecha_hora, function(x) substr(x, 1, 10)))
        temperaturas <- sapply(data$pronostico$temperatura, as.numeric)
        humedades <- sapply(data$pronostico$humedad, as.numeric)
        
        pronostico_df <- data.frame(
          Fecha = fechas,
          Temperatura = temperaturas,
          Humedad = humedades,
          stringsAsFactors = FALSE
        )
        
        promedios_diarios <- aggregate(cbind(Temperatura, Humedad) ~ Fecha,
                                       data = pronostico_df, FUN = mean)
        
        list(ciudad = ciudad, data = promedios_diarios)
      } else {
        list(ciudad = ciudad, data = NULL)
      }
    })
    
    # Renderizar tablas de pronóstico para cada ciudad
    output$tabla_pronostico <- renderUI({
      tablas <- lapply(pronosticos, function(pronostico) {
        if (is.null(pronostico$data)) {
          box(title = paste("Pronóstico para", pronostico$ciudad),
              status = "danger",
              solidHeader = TRUE,
              "No hay datos disponibles.")
        } else {
          fluidRow(
            column(12, h3(paste("Pronóstico para", pronostico$ciudad))),
            column(12, DT::datatable(pronostico$data, options = list(pageLength = 5)))
          )
        }
      })
      do.call(tagList, tablas)
    })
    
    # Renderizar gráficos de pronóstico para cada ciudad
    output$grafico_pronostico <- renderUI({
      graficos <- lapply(pronosticos, function(pronostico) {
        if (is.null(pronostico$data)) {
          NULL
        } else {
          sanitized_ciudad <- sanitize_id(pronostico$ciudad)
          plotlyOutput(paste0("grafico_pronostico_", sanitized_ciudad), height = "400px")
        }
      })
      do.call(tagList, graficos)
    })
    
    # Generar gráficos para cada pronóstico
    lapply(pronosticos, function(pronostico) {
      if (!is.null(pronostico$data)) {
        sanitized_ciudad <- sanitize_id(pronostico$ciudad)
        output[[paste0("grafico_pronostico_", sanitized_ciudad)]] <- renderPlotly({
          tryCatch({
            plot_ly(data = pronostico$data,
                    x = ~Fecha,
                    y = ~Temperatura,
                    type = 'scatter',
                    mode = 'lines+markers',
                    name = "Temperatura",
                    marker = list(color = 'blue')) %>%
              add_trace(y = ~Humedad, name = "Humedad", marker = list(color = 'lightblue')) %>%
              layout(
                title = paste("Pronóstico de Temperatura y Humedad para", pronostico$ciudad),
                xaxis = list(title = "Fecha", tickformat = "%d-%m"),
                yaxis = list(title = "Valor")
              )
          }, error = function(e) {
            print(paste("Error en grafico_pronostico:", e$message))
            NULL
          })
        })
      }
    })
  })
  
  # Limpiar los puntos seleccionados
  observeEvent(input$limpiar_puntos, {
    puntos_seleccionados(data.frame(
      Ciudad = character(),
      Temperatura = numeric(),
      Humedad = numeric(),
      Viento = character(),
      Latitud = numeric(),
      Longitud = numeric(),
      stringsAsFactors = FALSE
    ))  # Limpia los puntos seleccionados
    updateSelectInput(session, "combo_ciudad", selected = NULL)
    leafletProxy("mapa") %>% clearMarkers()
    
    # Limpiar indicadores y pronósticos
    output$indicadores_ui <- renderUI(NULL)
    output$grafico_pronostico <- renderUI(NULL)
    output$tabla_pronostico <- renderUI(NULL)
  })
  
  # Descargar Datos
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("puntos_clima", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(puntos_seleccionados(), file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv"
  )
  
  # Salida de depuración para verificar puntos_seleccionados (Opcional)
  output$debug_puntos <- renderPrint({
    puntos_seleccionados()
  })
})

