
library(httr)
library(jsonlite)

#' Check environmental migraine risk from weather (with env‐var API key)
#'
#' @param location  Character. City name (e.g. "Amsterdam") or "city,country".
#' @param api_key   Character. Your OpenWeatherMap API key.
#'                  Defaults to Sys.getenv("OWM_API_KEY").
#'
#' @return A list with:
#'   - weather_score: Numeric (0–3) environmental risk points
#'   - details: list(pressure, humidity, temp)
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(OWM_API_KEY = "your_own_key_here")
#' check_weather_risk("Amsterdam")
#' }
#'

check_weather_risk <- function(location, api_key = Sys.getenv("OWM_API_KEY")) {
  # Grab the key from env if not provided
  if (!nzchar(api_key)) {
    stop("No API key found. Please set the OWM_API_KEY environment variable.")
  }
  base_url <- "http://api.openweathermap.org/data/2.5/weather"
  response <- GET(
    base_url,
    query = list(
      q     = location,
      appid = api_key,
      units = "metric"
    )
  )

  # Handle HTTP errors
  if (status_code(response) != 200) {
    warning("Weather API request failed (status ", status_code(response),
            "). Check location or API key.")
    return(list(weather_score = 0, details = NULL))
  }

  # Parse JSON data
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  pressure <- data$main$pressure
  humidity <- data$main$humidity
  temp     <- data$main$temp

  # Score based on migraine‐trigger thresholds
  weather_score <- 0
  if (!is.null(pressure) && pressure < 1000)               weather_score <- weather_score + 1
  if (!is.null(humidity) && (humidity > 85 || humidity < 30)) weather_score <- weather_score + 1
  if (!is.null(temp)     && (temp >= 25 || temp <= 10))       weather_score <- weather_score + 1

  # 6) Return both the score and raw details
  list(
    weather_score = weather_score,
    details       = list(
      pressure = pressure,
      humidity = humidity,
      temp     = temp
    )
  )
}
# Test the function
result <- check_weather_risk("Berlin", "5754250f57228c5fa0fdf22fd68cb02e")
print(result)
