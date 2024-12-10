
library(shiny)
library(DT)
library(ggplot2)

fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

   tabsetPanel(
     tabPanel("Tablas DT",
              h1("Vista Básica"),
              fluidRow(
                column(12,dataTableOutput("tabla1"))
                ),
              h1("Formatear las celdas"),
              fluidRow(column(12,dataTableOutput("Tabla2")))
              ),
     tabPanel("Captura de click", 
              fluidRow(column(6, h2("Single Select Row"),
                              DT::dataTableOutput("Tabla3"),
                              verbatimTextOutput("output_t3")
                              ),
                       column(6,h2("Multiple Select Rows"),
                              DT::dataTableOutput("tabla4"),
                              verbatimTextOutput("output_t4")
                              )
                       )),
                      fluidRow(column(6, h2("Single Select Cols"),
                                      DT::dataTableOutput("Tabla5"),
                                      verbatimTextOutput("output_t5")
                                      ),
                                column(6, h2("Multi Select Cols"),
                                       DT::dataTableOutput("Tabla6"),
                                       verbatimTextOutput("output_t6")
                                      ),
                               column(6, h2("Single Select Cell"),
                                      DT::dataTableOutput("Tabla7"),
                                      verbatimTextOutput("output_t7")
                                      ),
                               column(6, h2("Multiple Select Cell"),
                                      DT::dataTableOutput("Tabla8"),
                                      verbatimTextOutput("output_t8")
                                      )
                               
                               
                      
                      )

     
   )
)
