library(lattice)
library(caret)
library(e1071)

#loading the datasets
phishing_data <- read.csv("dataset_phishing.csv")

# Select the desired columns
selected_columns <- phishing_data %>%
  select(
    length_url,ip, length_hostname, nb_dots, nb_hyphens, nb_slash, nb_www, ratio_digits_url, http_in_path, https_token,submit_email,
    popup_window,whois_registered_domain,domain_registration_length, dns_record,  google_index,  ratio_intRedirection,
    ratio_extRedirection,ratio_intErrors,ratio_extErrors, phish_hints, nb_redirection, nb_external_redirection, punycode,
    status, nb_qm, nb_and, nb_or, nb_eq, nb_underscore, nb_tilde, nb_percent, nb_star, nb_colon, nb_comma, nb_semicolumn, nb_dollar
    )


# Clean the selected data
cleaned_data <- selected_columns %>%
  na.omit() %>%
  distinct()
cleaned_data$status <- ifelse(cleaned_data$status == "phishing", 1, 0)


set.seed(123)
train_index <- caret::createDataPartition(cleaned_data$status, p = 0.8, list = FALSE)
train_data <- cleaned_data[train_index, -which(names(cleaned_data) == "status")]
test_data <- cleaned_data[-train_index, -which(names(cleaned_data) == "status")]
train_labels <- cleaned_data$status[train_index]
test_labels <- cleaned_data$status[-train_index]

# Train the model using SVM with a linear kernel
model <- svm(train_data, train_labels, kernel = "linear")

# Make predictions on the test data
predictions <- predict(model, test_data)


# Evaluate the model
confusion_matrix <- table(predictions, test_labels)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Check if the previously saved model exists
if (file.exists("phishing.rbs")) {
  # Load the previously saved model
  previous_model <- readRDS("phishing.rbs")
  
  # Check if the current model is more accurate than the previous model
  if (accuracy > previous_model$accuracy) {
    # Save the current model
    saveRDS(list(model = model, accuracy = accuracy), "phishing.rbs")
    message("New model saved.")
  } else {
    message("Existing model remains the best.")
  }
} else {
  # Save the model as no previous model is available
  saveRDS(list(model = model, accuracy = accuracy), "phishing.rbs")
  message("Model saved.")
}
