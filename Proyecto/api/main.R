# main.R
library(plumber)
library(reticulate)

# Configurar Python para cargar el modelo
use_python("/usr/bin/python3")  # Ajustar según la configuración

# Función global para cargar el modelo
load_model <- function() {
  tryCatch({
    pickle <- import("pickle")
    model <- pickle$load(py_open("models/model.pkl", "rb"))
    return(model)
  }, error = function(e) {
    stop("Error al cargar el modelo: ", e$message)
  })
}

# Función global para realizar predicciones
predict_default <- function(data) {
  model <- load_model()
  
  # Asegurar orden correcto de columnas
  data <- data[, c("Income", "Age", "Loan", "Loan_to_Income")]
  
  # Convertir a matriz numpy
  np <- import("numpy")
  X <- np$array(as.matrix(data))
  
  # Realizar predicción
  predictions <- model$predict(X)
  return(as.vector(predictions))
}

# Iniciar el servidor Plumber
pr <- plumber::plumb("api.R")
pr$run(host = "0.0.0.0", port = 3747)
