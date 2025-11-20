library(shiny)
library(nnet)

# Load dataset and prepare model (same as your code)
df <- read.csv("C:/Users/admn/OneDrive/Desktop/CUEA/CMT 429/Self-Medication.csv")

# Clean medicine_type into categories
df$medicine_clean <- sapply(df$medicine_type, function(x) {
  if (is.na(x)) return(NA)
  parts <- unlist(strsplit(x, ";"))
  parts <- trimws(parts)
  parts <- unique(parts)  # remove duplicates
  if (length(parts) == 1) {
    return(parts[1])
  } else {
    # Combine multiple drugs in alphabetical order
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

# Prepare model data
model_data <- df[, c("medicine_clean", factor_columns)]
model_data <- model_data[!is.na(model_data$medicine_clean), ]

# Train multinomial logistic regression model
drug_model <- multinom(
  medicine_clean ~ age_group + gender + education + employment_status +
    access_healthcare + consult_pharmacist + reasons_selfmed +
    monthly_spend + frequency,
  data = model_data
)


# Extract factor levels for UI
age_options        <- levels(model_data$age_group)
gender_options     <- levels(model_data$gender)
education_options  <- levels(model_data$education)
employment_options <- levels(model_data$employment_status)
access_options     <- levels(model_data$access_healthcare)
consult_options    <- levels(model_data$consult_pharmacist)
reasons_options    <- levels(model_data$reasons_selfmed)
spend_options      <- levels(model_data$monthly_spend)
frequency_options  <- levels(model_data$frequency)

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
