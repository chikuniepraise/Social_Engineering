library(dplyr)
library(caret)
# Load the saved model
loaded_model <- readRDS("phishing.rbs")

# Load the test data for prediction
new_data <- data.frame(
  length_url = 77,                # Example values for length_url feature
  ip =1, # Example values for ip feature
  length_hostname = 23,             # Example values for length_hostname feature
  nb_dots = 1,                        # Example values for nb_dots feature
  nb_hyphens = 0,                     # Example values for nb_hyphens feature
  # Include the remaining features with their corresponding example values
  nb_slash = 0,
  nb_www = 1,
  ratio_digits_url = 0.220779221,
  http_in_path = 0,
  https_token = 0,
  submit_email = 0,
  popup_window = 0,
  whois_registered_domain = 0,
  domain_registration_length = 0,
  dns_record = 0,
  google_index = 0,
  ratio_intRedirection = 0,
  ratio_extRedirection = 0,
  ratio_intErrors = 0,
  ratio_extErrors = 0,
  phish_hints = 0,
  nb_redirection = 0,
  nb_external_redirection = 0,
  punycode = 0,
  nb_qm = 0,
  nb_and = 0,
  nb_or = 0,
  nb_eq = 0,
  nb_underscore = 0,
  nb_tilde = 0,
  nb_percent = 0,
  nb_star =0,
  nb_colon = 0,
  nb_comma = 0,
  nb_semicolumn = 0,
  nb_dollar = 0
)
print(new_data)
# Make predictions on the new data using the loaded model
new_predictions <- predict(loaded_model$model, new_data)

if (new_predictions[1] >= 1) {
  print(paste("url is predicted to be phishing (label = 1)."))
} else {
  print(paste("is predicted not to be phishing (label = 0)."))
}
