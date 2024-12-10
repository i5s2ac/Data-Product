
library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    tabsetPanel("Plot",
      h1("Graficas en shiny"),
      plotOutput("grafica_base_r"),
      plotOutput("grafica_ggplot")
      ),
    tabPanel("Plot Interactions",
             "tab2"
             )

)
