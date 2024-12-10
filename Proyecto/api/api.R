# api.R 
library(plumber)
library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(reticulate)

# Configurar Python y dependencias
virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
py_install(c("scikit-learn", "pandas", "numpy"))

# Importar módulos Python
np <- import("numpy")
pd <- import("pandas")
pickle <- import("pickle") 
sklearn <- import("sklearn")

# Función para conectar a la base de datos
get_db_connection <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = "test",
    host = "localhost",
    port = 5432,
    user = "test",
    password = "test123"
  )
}

# Función para cargar el modelo activo desde la base de datos  
load_active_model <- function() {
  tryCatch({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    # Obtener el modelo activo más reciente
    query <- "
      SELECT model_data
      FROM dataproduct.models
      WHERE is_active = true
      ORDER BY created_at DESC 
      LIMIT 1;
    "
    
    result <- dbGetQuery(conn, query)
    
    if (nrow(result) == 0) {
      stop("No se encontró un modelo activo en la base de datos")
    }
    
    # Cargar el modelo desde los bytes
    model_bytes <- result$model_data[[1]]
    model <- pickle$loads(model_bytes)
    
    return(model)
  }, error = function(e) {
    stop(paste("Error al cargar el modelo:", e$message))
  })
}

# Función para crear la tabla de logs si no existe
create_logs_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataproduct.api_logs (
      id SERIAL PRIMARY KEY,
      timestamp TIMESTAMP,
      endpoint VARCHAR(100),
      method VARCHAR(10),
      input_data JSONB,
      output_data JSONB,
      response_time FLOAT,
      status_code INTEGER,
      error_message TEXT,
      ip_address VARCHAR(50),
      user_agent TEXT,
      prediction_confidence FLOAT,
      processing_time FLOAT
    );
  ")
}

# Crear la tabla de logs al iniciar la API
initialize_logs_table <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  create_logs_table(conn)
}

initialize_logs_table()

# Función para registrar logs
log_request <- function(timestamp, endpoint, method, input_data, output_data,
                        response_time, status_code, error_message = NULL,
                        request) {
  tryCatch({
    # Debug: Print logging information
    print(paste("Logging Request - Endpoint:", endpoint, "Method:", method))
    
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    # Insertar log
    sql <- "
      INSERT INTO dataproduct.api_logs
      (timestamp, endpoint, method, input_data, output_data,
       response_time, status_code, error_message, ip_address,
       user_agent, prediction_confidence, processing_time)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
    "
    
    # Obtener info adicional del request
    ip_address <- request$REMOTE_ADDR
    user_agent <- request$HTTP_USER_AGENT
    
    # Extraer confidence si existe
    prediction_confidence <- NULL
    if (!is.null(output_data$probability)) {
      # Para /predict
      prediction_confidence <- ifelse(length(output_data$probability) > 1, 
                                      max(output_data$probability), 
                                      output_data$probability)
    } else if (!is.null(output_data$probabilities)) {
      # Para /predict-batch
      # Asegurarse de que probabilities es un vector numérico
      prediction_confidence <- mean(unlist(output_data$probabilities))
    }
    
    # Manejar el caso cuando error_message es NULL
    if (is.null(error_message)) {
      error_message <- NA
    }
    
    params <- list(
      timestamp,
      endpoint,
      method,
      toJSON(input_data, auto_unbox = TRUE),
      toJSON(output_data, auto_unbox = TRUE),
      response_time,
      status_code,
      error_message,
      ip_address,
      user_agent,
      prediction_confidence,
      response_time  # Asumiendo que processing_time es igual a response_time
    )
    
    dbExecute(conn, sql, params)
    
    print("Log inserted successfully.")
    
  }, error = function(e) {
    warning(paste("Error en log_request:", e$message))
  })
}

# Iniciar el API plumber
#* @apiTitle Credit Default Prediction API
#* @apiDescription API para predicción de default crediticio

# Definir primero el endpoint más específico
#* @post /predict-batch
#* @param req Request body
function(req, res) {
  
  start_time <- Sys.time()
  
  tryCatch({
    # Declaración de depuración
    print("Inside /predict-batch handler")
    
    # Leer y parsear el cuerpo de la solicitud
    data <- jsonlite::fromJSON(req$postBody)
    data <- as.data.frame(data) 
    
    # Validar campos requeridos
    required_fields <- c("Income", "Age", "Loan") 
    if (!all(required_fields %in% names(data))) {
      stop("Faltan campos requeridos: Income, Age, Loan")
    }
    
    # Convertir a numérico  
    data <- data %>%
      mutate(
        Income = as.numeric(Income),
        Age = as.numeric(Age),
        Loan = as.numeric(Loan)
      )
    
    if (any(is.na(data$Income) | is.na(data$Age) | is.na(data$Loan))) {  
      stop("Los campos Income, Age y Loan deben ser numéricos")
    }
    
    data$Loan_to_Income <- data$Loan / data$Income
    
    model <- load_active_model()
    X <- np$array(as.matrix(data[, c("Income", "Age", "Loan", "Loan_to_Income")]))
    predictions <- model$predict(X)
    predictions_proba <- model$predict_proba(X)
    
    response <- list(
      status = "success",
      predictions = as.numeric(predictions),
      probabilities = apply(predictions_proba, 1, max),
      message = sprintf("Se realizaron %d predicciones exitosamente", nrow(data))
    )
    
    # Conectar a BD para guardar datos
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    # Agregar predicciones a dataframe
    data$Default <- as.numeric(predictions)
    
    dbWriteTable(
      conn,
      Id(schema="dataproduct", table="credit_data"),
      data,
      append=TRUE,
      row.names=FALSE  
    )
    
    # Registrar log exitoso  
    end_time <- Sys.time() 
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/predict-batch",
      method = "POST",
      input_data = data, 
      output_data = response, 
      response_time = response_time,
      status_code = 200,
      request = req
    )
    
    return(response)
    
  }, error = function(e) {
    res$status <- 500  
    error_response <- list(
      status = "error",
      message = paste("Error en predicciones batch:", e$message)
    )
    
    # Registrar log de error
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    # Manejar caso donde 'data' no está definido
    log_request(
      timestamp = start_time,
      endpoint = "/predict-batch", 
      method = "POST",
      input_data = if(exists("data")) data else list(),
      output_data = error_response,
      response_time = response_time,
      status_code = 500,
      error_message = e$message,
      request = req
    )
    
    return(error_response)
  })  
}

#* @post /predict
#* @param Income Ingreso del cliente
#* @param Age Edad del cliente  
#* @param Loan Monto del préstamo
function(Income, Age, Loan, req, res) {
  
  start_time <- Sys.time()
  
  tryCatch({
    # Declaración de depuración
    print("Inside /predict handler")
    
    data <- data.frame(
      Income = as.numeric(Income), 
      Age = as.numeric(Age),
      Loan = as.numeric(Loan)  
    )
    
    data$Loan_to_Income <- data$Loan / data$Income
    
    model <- load_active_model()
    X <- np$array(as.matrix(data[, c("Income", "Age", "Loan", "Loan_to_Income")]))
    prediction <- model$predict(X)
    prediction_proba <- model$predict_proba(X)
    
    response <- list(
      status = "success",
      prediction = as.numeric(prediction[1]),
      probability = as.numeric(prediction_proba[1, prediction[1]+1]),
      message = "Predicción realizada exitosamente"
    )
    
    # Conectar a BD para guardar datos  
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    # Agregar predicción a dataframe  
    data$Default <- as.numeric(prediction[1]) 
    
    dbWriteTable(
      conn, 
      Id(schema="dataproduct", table="credit_data"),
      data,
      append=TRUE,
      row.names=FALSE
    )
    
    # Registrar log exitoso
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/predict", 
      method = "POST",
      input_data = list(
        Income = Income,
        Age = Age,
        Loan = Loan
      ),
      output_data = response,
      response_time = response_time,
      status_code = 200,
      request = req  
    )
    
    return(response)
    
  }, error = function(e) {
    res$status <- 500
    error_response <- list(
      status = "error",
      message = paste("Error en predicción:", e$message)  
    )
    
    # Registrar log de error
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/predict",
      method = "POST", 
      input_data = list(
        Income = Income,
        Age = Age, 
        Loan = Loan
      ),
      output_data = error_response,
      response_time = response_time,
      status_code = 500,
      error_message = e$message,
      request = req
    )
    
    return(error_response)  
  })
}

#* @post /update-model
#* @param model El archivo del modelo (.pkl) 
function(model, req, res) {
  
  start_time <- Sys.time()
  
  tryCatch({
    
    if (is.null(model)) {
      res$status <- 400
      return(list(
        status = "error",
        message = "No se recibió archivo de modelo"
      ))
    }
    
    # Leer el archivo del modelo
    model_data <- readBin(model$datapath, "raw", n=file.info(model$datapath)$size)
    
    # Verificar que el modelo se puede cargar  
    model_loaded <- pickle$loads(model_data)
    
    # Conectar a la base de datos
    conn <- get_db_connection()  
    on.exit(dbDisconnect(conn))
    
    # Desactivar modelos anteriores
    dbExecute(conn, "UPDATE dataproduct.models SET is_active = false WHERE is_active = true")
    
    # Insertar nuevo modelo
    query <- "
      INSERT INTO dataproduct.models
      (model_data, created_at, is_active, description)  
      VALUES ($1, NOW(), true, $2)  
    "
    
    dbExecute(
      conn,
      query,
      list(
        model_data,
        paste("Modelo actualizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))  
      ) 
    )
    
    # Registrar log exitoso
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/update-model", 
      method = "POST",
      input_data = list(model = "Uploaded"),
      output_data = list(status = "success", message = "Modelo actualizado exitosamente"),
      response_time = response_time,
      status_code = 200,
      request = req  
    )
    
    return(list(
      status = "success",
      message = "Modelo actualizado exitosamente"  
    ))
    
  }, error = function(e) {
    res$status <- 500
    error_response <- list(
      status = "error",
      message = paste("Error al actualizar modelo:", e$message)
    )
    
    # Registrar log de error
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/update-model",
      method = "POST",
      input_data = list(model = "Uploaded"),
      output_data = error_response,
      response_time = response_time,
      status_code = 500,
      error_message = e$message,
      request = req
    )
    
    return(error_response)
  })
}

#* @get /model-info  
function(req, res) {
  
  start_time <- Sys.time()
  
  tryCatch({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    query <- "
      SELECT id, train_score, test_score, created_at, description
      FROM dataproduct.models  
      WHERE is_active = true
      ORDER BY created_at DESC
      LIMIT 1;
    "
    
    model_info <- dbGetQuery(conn, query)
    
    if (nrow(model_info) == 0) {
      res$status <- 404 
      output_data <- list(
        status = "error", 
        message = "No hay modelo activo"
      )
      
      # Registrar log de error
      end_time <- Sys.time()
      response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
      
      log_request(
        timestamp = start_time,
        endpoint = "/model-info",
        method = "GET",
        input_data = list(),
        output_data = output_data,
        response_time = response_time,
        status_code = 404,
        request = req
      )
      
      return(output_data)
    } 
    
    output_data <- list(
      status = "success",
      model_info = as.list(model_info[1,]) 
    )
    
    # Registrar log exitoso
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/model-info",
      method = "GET",
      input_data = list(),
      output_data = output_data, 
      response_time = response_time,
      status_code = 200,
      request = req
    )
    
    return(output_data)
    
  }, error = function(e) {
    res$status <- 500
    error_response <- list(  
      status = "error",
      message = paste("Error al obtener info del modelo:", e$message)
    )
    
    # Registrar log de error
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units="secs"))
    
    log_request(
      timestamp = start_time,
      endpoint = "/model-info", 
      method = "GET",
      input_data = list(),
      output_data = error_response,
      response_time = response_time,
      status_code = 500,
      error_message = e$message,
      request = req
    )
    
    return(error_response)
  })
}
