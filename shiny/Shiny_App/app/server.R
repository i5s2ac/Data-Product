#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$tabla1 <- DT::renderDataTable({
      mtcars$model <- rownames(mtcars) 
      mtcars %>% DT::datatable(rownames = FALSE, 
                               filter = "top",
                               options = list(buttons = c("csv"),
                                              pageLenght = 5,
                                              lenghtMenu = c(-1,5,10,15)))
    })
    
    output$Tabla2 <- DT::renderDataTable({
      diamonds %>%
        mutate(vol=x*y*z,
               vol_promedio = mean(vol),
               volp = vol/vol_promedio-1) %>%
        DT::datatable( filter = "top" ) %>% 
        formatCurrency(columns="price", currency = "$") %>%
        formatPercentage(columns= "volp", digits=2)
    })
    
    
    output$Tabla3 <- DT::renderDataTable({
      mtcars %>% DT::datatable(selection = "single") 
    })
    
    output$output_t3 <- renderPrint({
      fila3 = input$Tabla3_rows_selected
      mtcars[fila3,]
    })
    
    output$tabla4 <- DT::renderDataTable({
      mtcars %>% DT::datatable() 
    })
    
    output$output_t4 <- renderPrint({
      fila4 =input$tabla4_rows_selected
      mtcars[fila4,]
      
    })
    
    output$Tabla5 <- DT::renderDataTable({
      mtcars %>% DT::datatable(selection = list(mode="single",
                                                target = "column")) 
    })
    
    output$output_t5 <- renderPrint({
      row5= input$Tabla5_columns_selected
      mtcars[row5,]
      
    })
    
    output$Tabla6 <- DT::renderDataTable({
      mtcars %>% DT::datatable(selection = list(mode="multiple",
                                                target = "column")) 
    })
    
    output$output_t6 <- renderPrint({
      row6 = input$Tabla6_columns_selected
      mtcars[row6,]
      
    })
    
    output$Tabla7 <- DT::renderDataTable({
      mtcars %>% DT::datatable(selection = list(mode="single",
                                                target = "cell")) 
    })
    
    output$output_t7 <- renderPrint({
      cell7 <- input$Tabla7_cells_selected
      
      if (!is.null(cell7) && length(cell7) == 2) {
        selected_row <- cell7[1, 1]  # Detectar la fila seleccionada
        selected_col <- cell7[1, 2]  # Detectar la columna seleccionada
        
        car_name <- rownames(mtcars)[selected_row]  # Nombre del carro
        column_name <- colnames(mtcars)[selected_col]  # Nombre de la columna 
        selected_value <- mtcars[selected_row, selected_col]  # Valor seleccionado
        
        cat("Car:", car_name, "\nMetric:", column_name, "\nValue:", selected_value)
      } else {
        cat("No se ha seleccionado ninguna celda.")
      }
    })
    
    output$Tabla8 <- DT::renderDataTable({
      mtcars %>% DT::datatable(selection = list(mode="multiple",
                                                target = "cell")) 
    })
    
    output$output_t8 <- renderPrint({
      selected_cells <- input$Tabla8_cells_selected
      
      if (!is.null(selected_cells) && nrow(selected_cells) > 0) {
        for (i in 1:nrow(selected_cells)) {
          selected_row <- selected_cells[i, 1]  # Detectar la fila seleccionada
          selected_col <- selected_cells[i, 2]  # Detectar la columna seleccionada
          
          car_name <- rownames(mtcars)[selected_row]  # Nombre del carro
          column_name <- colnames(mtcars)[selected_col]  # Nombre de la columna 
          selected_value <- mtcars[selected_row, selected_col]  # Valor seleccionado
          
          # Imprimir cada selección en un formato "bonito"
          cat("Car:", car_name, "\nMetric:", column_name, "\nValue:", selected_value, "\n\n")
        }
      } else {
        cat("No se ha seleccionado ninguna celda.")
      }
    })
    
    
}


