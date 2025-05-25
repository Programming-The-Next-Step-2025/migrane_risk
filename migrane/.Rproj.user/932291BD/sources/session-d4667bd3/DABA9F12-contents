#' Evaluate biological migraine risk factors
#'
#' This function calculates a biological risk score based on daily habits and menstruation cycle.
#'
#' @param migraines_month Integer. Number of migraines in the past month.
#' @param sleep_hours Numeric. Hours slept last night.
#' @param sleep_quality Character. "good" or "poor".
#' @param alcohol Logical. Did the user drink alcohol in the last 24 hours?
#' @param red_wine Logical. Did they drink red wine? (Only if alcohol is TRUE)
#' @param gender Character. User's gender ("female", "male", etc.).
#' @param last_period_date Date. Date of last menstrual period (if female).
#' @param cycle_length Integer. Typical cycle length in days (if female).
#' @param water_liters Numeric. How much water did they drink today? (liters)
#' @param caffeine_change Character. One of: "less", "normal", or "more".
#' @param hours_since_last_meal Numeric. How many hours since their last meal?
#'
#' @return A list with the biological risk score, number of migraines, and cycle information (if applicable).
#' @export
#'
#' @examples
#' evaluate_biological_factors(
#'   migraines_month = 3,
#'   sleep_hours = 6,
#'   sleep_quality = "poor",
#'   alcohol = TRUE,
#'   red_wine = FALSE,
#'   gender = "female",
#'   last_period_date = as.Date("2025-05-01"),
#'   cycle_length = 28,
#'   water_liters = 1.5,
#'   caffeine_change = "more",
#'   hours_since_last_meal = 6
#' )
evaluate_biological_factors <- function(
    migraines_month,
    sleep_hours,
    sleep_quality,
    alcohol,
    red_wine,
    gender,
    last_period_date = NA,
    cycle_length = NA,
    water_liters,
    caffeine_change,
    hours_since_last_meal
) {
  score <- 0

  # Sleep
  if (sleep_hours < 7) score <- score + 1
  if (tolower(sleep_quality) == "poor") score <- score + 1

  # Alcohol and red wine
  if (isTRUE(alcohol)) {
    score <- score + 1
    if (isTRUE(red_wine)) score <- score + 0.5
  }

  # Water intake
  if (water_liters < 2) score <- score + 1

  # Caffeine deviation
  if (tolower(caffeine_change) != "normal") score <- score + 1

  # Meal timing
  if (hours_since_last_meal > 5) score <- score + 1

  # Menstruation tracking and risk adjustment
  cycle_info <- NULL
  if (tolower(gender) == "female" && !is.na(last_period_date) && !is.na(cycle_length)) {
    today <- Sys.Date()
    days_since_last <- as.integer(today - last_period_date)
    day_in_cycle <- (days_since_last %% cycle_length) + 1
    next_period_start <- last_period_date + cycle_length * (days_since_last %/% cycle_length + 1)
    cycle_info <- list(
      last_period_date = last_period_date,
      cycle_length = cycle_length,
      estimated_next_period = next_period_start,
      day_in_cycle = day_in_cycle
    )

    # Add +1 if within 3 days before period or during first 5 days
    if (day_in_cycle >= (cycle_length - 3) || day_in_cycle <= 5) {
      score <- score + 1
    }
  }

  return(list(
    biological_score = score,
    migraines_this_month = migraines_month,
    cycle_info = cycle_info
  ))
}
