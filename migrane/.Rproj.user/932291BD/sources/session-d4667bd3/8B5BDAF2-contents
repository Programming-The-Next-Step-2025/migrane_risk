#' Create a user profile
#'
#' This function collects user information and calculates a baseline migraine score.
#'
#' @param name Character. User's name.
#' @param gender Character. User's gender ("female", "male", "nonbinary").
#' @param age Numeric. User's age in years.
#' @param family_history Logical. Family history of migraines? (TRUE/FALSE)
#' @param location Character. User's location.
#'
#' @return A list with baseline_score and location.
#' @export
#'
#' @examples
#' create_user_profile("Alex", "female", 26, TRUE, "Amsterdam")
create_user_profile <- function(name, gender, age, family_history, location) {
  baseline_score <- 0

  # Check gender
  if (tolower(gender) == "female") {
    baseline_score <- baseline_score + 1
  }

  # logical for family history ( true/false or yes/no)
  if (isTRUE(family_history) || tolower(family_history) == "yes") {
    baseline_score <- baseline_score + 1
  }

  # Age range scoring
  if (age >= 13 && age <= 51) {
    baseline_score <- baseline_score + 1
  }

  # Return score and location
  return(list(
    baseline_score = baseline_score,
    location = location
  ))
}
