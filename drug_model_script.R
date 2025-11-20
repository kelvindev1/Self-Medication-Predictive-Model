library(nnet)

# Load dataset and prepare model
df <- read.csv("Self-Medication.csv")

df$medicine_clean <- sapply(df$medicine_type, function(x) {
  if (is.na(x)) return(NA)
  parts <- unlist(strsplit(x, ";"))
  parts <- trimws(parts)
  parts <- unique(parts)
  if (length(parts) == 1) return(parts[1])
  return(paste(sort(parts), collapse = " + "))
})
df$medicine_clean <- factor(df$medicine_clean)

factor_columns <- c(
  "age_group","gender","education","employment_status",
  "access_healthcare","consult_pharmacist","reasons_selfmed",
  "monthly_spend","frequency"
)
for (col in factor_columns) {
  df[[col]] <- factor(df[[col]])
}

model_data <- df[, c("medicine_clean", factor_columns)]
model_data <- model_data[!is.na(model_data$medicine_clean), ]

# Train the model
drug_model <- multinom(
  medicine_clean ~ age_group + gender + education + employment_status +
    access_healthcare + consult_pharmacist + reasons_selfmed +
    monthly_spend + frequency,
  data = model_data
)

# Save the model as an RDS file
saveRDS(drug_model, "drug_model.rds")

