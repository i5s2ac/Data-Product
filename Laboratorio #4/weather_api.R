# Cargar librerías necesarias
library(plumber)
library(httr)
library(jsonlite)

# Definir tu API key de OpenWeather
api_key <- "5c43d19da16c565f01c17e917f1bd85e"

#* @apiTitle OpenWeather API en R
#* @apiDescription API en R usando plumber para consultar el clima desde OpenWeather.

# Función para obtener datos del clima por ciudad
get_weather_by_city <- function(ciudad, units = "metric") {
  base_url <- "http://api.openweathermap.org/data/2.5/weather"
  
  response <- GET(base_url, query = list(
    q = ciudad,
    appid = api_key,
    units = units,
    lang = "es"
  ))
  
  return(content(response, "parsed"))
}

# Función para obtener pronóstico de 5 días por ciudad
get_forecast_by_city <- function(ciudad, units = "metric") {
  base_url <- "http://api.openweathermap.org/data/2.5/forecast"
  
  response <- GET(base_url, query = list(
    q = ciudad,
    appid = api_key,
    units = units,
    lang = "es"
  ))
  
  return(content(response, "parsed"))
}

#* Obtener clima de una ciudad
#* @param ciudad El nombre de la ciudad que quieres consultar
#* @param unidades (opcional) Unidades: metric (Celsius), imperial (Fahrenheit), standard (Kelvin)
#* @get /clima
function(ciudad, unidades = "metric") {
  
  weather_data <- get_weather_by_city(ciudad, unidades)
  
  # Verificar que la respuesta es exitosa
  if (!is.null(weather_data$main)) {
    list(
      ciudad = weather_data$name,
      descripcion = weather_data$weather[[1]]$description,
      temperatura = weather_data$main$temp,
      sensacion_termica = weather_data$main$feels_like,
      humedad = weather_data$main$humidity,
      viento = list(
        velocidad = weather_data$wind$speed,
        direccion = weather_data$wind$deg
      ),
      presion = weather_data$main$pressure,
      amanecer = as.POSIXct(weather_data$sys$sunrise, origin="1970-01-01", tz="GMT"),
      atardecer = as.POSIXct(weather_data$sys$sunset, origin="1970-01-01", tz="GMT")
    )
  } else {
    list(
      error = "No se pudo obtener la información meteorológica. Verifica el nombre de la ciudad."
    )
  }
}

#* Obtener pronóstico de 5 días para una ciudad
#* @param ciudad El nombre de la ciudad
#* @param unidades (opcional) Unidades: metric (Celsius), imperial (Fahrenheit), standard (Kelvin)
#* @get /pronostico
function(ciudad, unidades = "metric") {
  
  forecast_data <- get_forecast_by_city(ciudad, unidades)
  
  # Verificar que la respuesta es exitosa
  if (!is.null(forecast_data$list)) {
    pronostico <- lapply(forecast_data$list, function(entry) {
      list(
        fecha_hora = entry$dt_txt,
        descripcion = entry$weather[[1]]$description,
        temperatura = entry$main$temp,
        sensacion_termica = entry$main$feels_like,
        humedad = entry$main$humidity,
        viento = list(
          velocidad = entry$wind$speed,
          direccion = entry$wind$deg
        )
      )
    })
    list(
      ciudad = forecast_data$city$name,
      pronostico = pronostico
    )
  } else {
    list(
      error = "No se pudo obtener el pronóstico. Verifica el nombre de la ciudad."
    )
  }
}

#* Obtener clima por coordenadas (latitud y longitud)
#* @param lat Latitud
#* @param lon Longitud
#* @param unidades (opcional) Unidades: metric (Celsius), imperial (Fahrenheit), standard (Kelvin)
#* @get /clima_coordenadas
function(lat, lon, unidades = "metric") {
  base_url <- "http://api.openweathermap.org/data/2.5/weather"
  
  response <- GET(base_url, query = list(
    lat = lat,
    lon = lon,
    appid = api_key,
    units = unidades,
    lang = "es"
  ))
  
  weather_data <- content(response, "parsed")
  
  if (!is.null(weather_data$main)) {
    list(
      ciudad = weather_data$name,
      descripcion = weather_data$weather[[1]]$description,
      temperatura = weather_data$main$temp,
      sensacion_termica = weather_data$main$feels_like,
      humedad = weather_data$main$humidity,
      viento = list(
        velocidad = weather_data$wind$speed,
        direccion = weather_data$wind$deg
      ),
      presion = weather_data$main$pressure,
      amanecer = as.POSIXct(weather_data$sys$sunrise, origin="1970-01-01", tz="GMT"),
      atardecer = as.POSIXct(weather_data$sys$sunset, origin="1970-01-01", tz="GMT")
    )
  } else {
    list(
      error = "No se pudo obtener la información meteorológica. Verifica las coordenadas."
    )
  }
}