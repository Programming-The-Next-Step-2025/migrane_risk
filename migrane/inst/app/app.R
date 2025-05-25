library(shiny)
library(migrane)  # Load your package to access its functions

ui <- fluidPage(
  titlePanel("Migraine Risk Tracker"),

  tabsetPanel(
    id = "tabs",

    # User Profile
    tabPanel("User profile",
             sidebarLayout(
               sidebarPanel(
                 textInput("name", "What is your name?"),
                 selectInput("gender", "Gender:", c("female", "male", "nonbinary")),
                 numericInput("age", "Age:", value = 30, min = 1, max = 120),
                 radioButtons("family_history", "Do you have a family history of migraines?",
                              choices = c("Yes" = TRUE, "No" = FALSE)),
                 textInput("location", "In what city do you live? "),
                 actionButton("submit_user", "Save Profile")
               ),
               mainPanel(
                 verbatimTextOutput("user_summary")
               )
             )
    ),

    # STEP 2: Daily Biological Factors
    tabPanel("Step 2: Daily Check-in",
             sidebarLayout(
               sidebarPanel(
                 numericInput("migraines", "Migraines this month:", 0, min = 0),
                 numericInput("sleep_hours", "Hours of sleep last night:", 8, min = 0),
                 selectInput("sleep_quality", "Sleep quality:", c("good", "poor")),
                 radioButtons("alcohol", "Did you drink alcohol in the last 24h?", c("Yes" = TRUE, "No" = FALSE)),
                 radioButtons("red_wine", "Did you drink red wine?", c("Yes" = TRUE, "No" = FALSE)),
                 numericInput("water", "Liters of water today:", 2),
                 selectInput("caffeine_change", "Caffeine compared to usual:", c("less", "normal", "more")),
                 numericInput("last_meal", "Hours since last meal:", 3),
                 uiOutput("menstrual_inputs"),
                 actionButton("submit_bio", "Evaluate Today")
               ),
               mainPanel(
                 verbatimTextOutput("bio_summary")
               )
             )
    )
  )
)

server <- function(input, output, session) {

  # Save user info using create_user_profile()
  user_info <- eventReactive(input$submit_user, {
    updateTabsetPanel(session, "tabs", selected = "Step 2: Daily Check-in")
    migrane::create_user_profile(
      name = input$name,
      gender = input$gender,
      age = input$age,
      family_history = as.logical(input$family_history),
      location = input$location
    )
  })

  output$user_summary <- renderPrint({
    req(user_info())
    cat("User:", user_info()$name, "\n")
    cat("Gender:", user_info()$gender, "\n")
    cat("Location:", user_info()$location, "\n")
    cat("Baseline migraine score:", user_info()$baseline_score, "\n")
  })

  output$menstrual_inputs <- renderUI({
    req(user_info())
    if (tolower(user_info()$gender) == "female") {
      tagList(
        dateInput("last_period", "Last period date:"),
        numericInput("cycle_length", "Cycle length (days):", value = 28, min = 20, max = 35)
      )
    }
  })

  bio_score <- eventReactive(input$submit_bio, {
    req(user_info())
    migrane::evaluate_biological_factors(
      migraines_month = input$migraines,
      sleep_hours = input$sleep_hours,
      sleep_quality = input$sleep_quality,
      alcohol = as.logical(input$alcohol),
      red_wine = as.logical(input$red_wine),
      gender = user_info()$gender,
      last_period_date = input$last_period,
      cycle_length = input$cycle_length,
      water_liters = input$water,
      caffeine_change = input$caffeine_change,
      hours_since_last_meal = input$last_meal
    )
  })

  output$bio_summary <- renderPrint({
    req(user_info(), bio_score())
    total <- user_info()$baseline_score + bio_score()$biological_score
    cat("Your baseline risk score:", user_info()$baseline_score, "\n")
    cat("Today's biological risk score:", bio_score()$biological_score, "\n")
    cat("TOTAL risk score today:", total, "\n")
    if (!is.null(bio_score()$cycle_info)) {
      cat("Next period estimated:", bio_score()$cycle_info$estimated_next_period, "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
