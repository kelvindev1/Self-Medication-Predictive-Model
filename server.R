library(shiny)
library(nnet)
library(rsconnect)


# Load saved model
drug_model <- readRDS("drug_model.rds")

# Load dataset to extract factor levels for UI
df <- read.csv("Self-Medication.csv")

# Clean medicine_type into categories
df$medicine_clean <- sapply(df$medicine_type, function(x) {
  if (is.na(x)) return(NA)
  parts <- unlist(strsplit(x, ";"))
  parts <- trimws(parts)
  parts <- unique(parts)  # remove duplicates
  if (length(parts) == 1) {
    return(parts[1])
  } else {
    return(paste(sort(parts), collapse = " + "))
  }
})
df$medicine_clean <- factor(df$medicine_clean)

# Convert predictors to factors
factor_columns <- c(
  "age_group","gender","education","employment_status",
  "access_healthcare","consult_pharmacist","reasons_selfmed",
  "monthly_spend","frequency"
)
for (col in factor_columns) {
  df[[col]] <- factor(df[[col]])
}

# Extract factor levels for UI
age_options        <- levels(df$age_group)
gender_options     <- levels(df$gender)
education_options  <- levels(df$education)
employment_options <- levels(df$employment_status)
access_options     <- levels(df$access_healthcare)
consult_options    <- levels(df$consult_pharmacist)
reasons_options    <- levels(df$reasons_selfmed)
spend_options      <- levels(df$monthly_spend)
frequency_options  <- levels(df$frequency)

# Shiny server
shinyServer(function(input, output, session) {
  
  # Update UI choices dynamically
  updateSelectInput(session, "age_group", choices = age_options)
  updateSelectInput(session, "gender", choices = gender_options)
  updateSelectInput(session, "education", choices = education_options)
  updateSelectInput(session, "employment_status", choices = employment_options)
  updateSelectInput(session, "access_healthcare", choices = access_options)
  updateSelectInput(session, "consult_pharmacist", choices = consult_options)
  updateSelectInput(session, "reasons_selfmed", choices = reasons_options)
  updateSelectInput(session, "monthly_spend", choices = spend_options)
  updateSelectInput(session, "frequency", choices = frequency_options)
  
  # Predict on button click
  observeEvent(input$predict_btn, {
    user_input <- data.frame(
      age_group = factor(input$age_group, levels = age_options),
      gender = factor(input$gender, levels = gender_options),
      education = factor(input$education, levels = education_options),
      employment_status = factor(input$employment_status, levels = employment_options),
      access_healthcare = factor(input$access_healthcare, levels = access_options),
      consult_pharmacist = factor(input$consult_pharmacist, levels = consult_options),
      reasons_selfmed = factor(input$reasons_selfmed, levels = reasons_options),
      monthly_spend = factor(input$monthly_spend, levels = spend_options),
      frequency = factor(input$frequency, levels = frequency_options)
    )
    
    prediction <- predict(drug_model, user_input)
    
    output$prediction_text <- renderText({
      paste("The model predicts you are most likely to misuse:", prediction)
    })
  })
})


# To deploy incase of changes use 
#rsconnect::deployApp(appName = "Self-Medication-Predictor")

# Steps to deploy

# install.packages("shiny")
# install.packages("nnet")
# install.packages("rsconnect")

# setwd("C:/Users/admn/OneDrive/Documents/Self-Medication-Model")

# rsconnect::setAccountInfo(name='kelvin-dev1', token='37A9F4436CF47A5727FC66564A78E07A', secret='CKgBvZcvEZgcaOSf9MAoLkQWgPCiM5+uYL7AupVQ')

# Test Locally
# shiny::runApp()

# Deploy
# rsconnect::deployApp(appName = "Self-Medication-Predictor")