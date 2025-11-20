library(shiny)

shinyUI(fluidPage(
  titlePanel("Self-Medication Drug Misuse Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group", "Select Age Group:", choices = NULL),
      selectInput("gender", "Select Gender:", choices = NULL),
      selectInput("education", "Select Education Level:", choices = NULL),
      selectInput("employment_status", "Select Employment Status:", choices = NULL),
      selectInput("access_healthcare", "Access to Healthcare:", choices = NULL),
      selectInput("consult_pharmacist", "Consult Pharmacist:", choices = NULL),
      selectInput("reasons_selfmed", "Reason for Self-Medication:", choices = NULL),
      selectInput("monthly_spend", "Monthly Spend:", choices = NULL),
      selectInput("frequency", "Frequency of Self-Medication:", choices = NULL),
      actionButton("predict_btn", "Predict")
    ),
    
    mainPanel(
      h3("Prediction Result:"),
      verbatimTextOutput("prediction_text")
    )
  )
))
