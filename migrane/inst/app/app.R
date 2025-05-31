library(shiny)
library(migrane)  # for create_user_profile
library(shinythemes)

# Source local functions
source("../../R/biological_factors_function.R")  # defines evaluate_biological_factors()
source("../../R/weather_score_function.R")       # defines check_weather_risk()

# UI Definition with Custom Styles
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "      /* Page background and text */
      body { background-color: #BDE0FE; color: #4A5759; }

      /* Buttons */
      .btn, .btn-default { background-color: #CDB4DB !important; color: #4A5759 !important; border-color: #CDB4DB !important; }
      .btn:hover, .btn:focus { background-color: #CDB4DB !important; }

      /* Standard inputs */
      .form-control { background-color: #FFC8DD !important; color: #4A5759 !important; border: 1px solid #4A5759 !important; }

      /* Selectize (dropdown) styling */
      .selectize-input { background-color: #FFC8DD !important; color: #4A5759 !important; border: 1px solid #4A5759 !important; }
      .selectize-dropdown-content { background-color: #FFC8DD !important; color: #4A5759 !important; }

      /* Panels/wells */
      .panel, .well { background-color: #CDB4DB !important; }
"    ))
  ),

  titlePanel("Migraine Risk Tracker"),

  tabsetPanel(
    id = "tabs",

    # Step 1: User Profile with image
    tabPanel(
      "Step 1: User Profile",
      fluidRow(
        column(6,
               textInput("name", "Name"),
               selectInput("gender", "Gender",
                           choices = c("female", "male", "nonbinary")),
               numericInput("age", "Age", value = 30, min = 0, max = 120),
               checkboxInput("family_history", "Family history of migraines?", FALSE),
               textInput("location", "Location", value = "Amsterdam"),
               actionButton("submit_user", "Save Profile")
        ),
        column(6,
               tags$img(
                 src   = "migraine_awareness.png",
                 style = "width:100%; height:auto; margin-top:20px;"
               )
        )
      )
    ),

    # Step 2: Daily Check-in with image
    tabPanel(
      "Step 2: Daily Check-in",
      fluidRow(
        column(6,
               numericInput("migraines", "Number of migraines this month", value = 0, min = 0),
               numericInput("sleep_hours", "Hours slept last night", value = 8, min = 0, max = 24),
               radioButtons("sleep_quality", "Sleep quality",
                            choices = c("good", "poor"), selected = "good"),
               checkboxInput("alcohol", "Drank alcohol in last 24h?", FALSE),
               conditionalPanel(
                 "input.alcohol == true",
                 checkboxInput("red_wine", "Was it red wine?", FALSE)
               ),
               numericInput("water", "Water intake (L)", value = 2, min = 0),
               radioButtons("caffeine_change", "Caffeine change",
                            choices = c("less", "normal", "more"), selected = "normal"),
               numericInput("last_meal", "Hours since last meal", value = 2, min = 0),
               actionButton("submit_bio", "Save Daily Check-in")
        ),
        column(6,
               tags$img(
                 src   = "check_in_png.png",
                 style = "width:100%; height:auto; margin-top:20px;"
               )
        )
      )
    ),
    # Step 3: Environmental Check
    tabPanel(
      "Step 3: Environmental Check",
      sidebarLayout(
        sidebarPanel(
          actionButton("check_weather", "Check Environmental Risk")
        ),
        mainPanel(
          # Wrap summary in colored div
          tags$div(
            style = "background-color: #FFC8DD; padding: 15px; border-radius: 5px;",
            verbatimTextOutput("weather_summary")
          )
        )
      )
    )
  )
)
# Server Logic
server <- function(input, output, session) {

  # 1. Build user profile
  user_info <- eventReactive(input$submit_user, {
    migrane::create_user_profile(
      name = input$name,
      gender = input$gender,
      age = input$age,
      family_history = input$family_history,
      location = input$location
    )
  })

  #Calculate biological risk
  bio_score <- eventReactive(input$submit_bio, {
    evaluate_biological_factors(
      migraines_month       = input$migraines,
      sleep_hours           = input$sleep_hours,
      sleep_quality         = input$sleep_quality,
      alcohol               = input$alcohol,
      red_wine              = if (input$alcohol) input$red_wine else FALSE,
      water_liters          = input$water,
      caffeine_change       = input$caffeine_change,
      hours_since_last_meal = input$last_meal
    )
  })

  # environmental risk
  weather_score <- eventReactive(input$check_weather, {
    req(user_info(), bio_score())
    check_weather_risk(user_info()$location, "5754250f57228c5fa0fdf22fd68cb02e")
  })

  # Output summary including greeting, scores, and weather details
  output$weather_summary <- renderPrint({
    req(user_info(), bio_score(), weather_score())

    # Greeting using the user's name input
    cat("Eyyy, ", input$name, "! Here is your migraine risk summary for today:\n\n", sep = "")

    # Extract scores
    base_score    <- user_info()$baseline_score
    bio_score_val <- bio_score()$biological_score
    env_score     <- weather_score()$weather_score
    total_score   <- base_score + bio_score_val + env_score

    # Extract weather parameters
    pressure <- weather_score()$details$pressure
    humidity <- weather_score()$details$humidity
    temp     <- weather_score()$details$temp

    # Print component scores
    cat("Baseline score:           ", base_score, "\n")
    cat("Biological factors score: ", bio_score_val, "\n")
    cat("Environmental score:      ", env_score, "\n")
    cat("-------------------------------\n")
    cat("TOTAL migraine risk score:", total_score, "\n\n")

    # Print weather details
    cat("Weather details:\n")
    cat("  • Pressure:    ", pressure, " hPa\n")
    cat("  • Humidity:    ", humidity, "%\n")
    cat("  • Temperature: ", temp, " °C\n\n")

    # threshold warnings
    high_threshold <- 9  # 14 max -5
    moderate_threshold <- 6
    if (total_score >= high_threshold) {
      cat("Uh oh, ", input$name, " High migraine risk today! Chill out!\n", sep = "")
    } else if (total_score >= moderate_threshold) {
      cat("Moderate risk today, ", input$name, ". Don't get triggered.\n", sep = "")
    } else {
      cat("All clear, ", input$name, ". Your risk is low today.\n", sep = "")
    }
  })
}

# Run App
shinyApp(ui, server)
