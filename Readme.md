# Social Engineering Detection using Machine Learning


*Prepared by: [Praise]*

*7/18/2023*

**1. Introduction**
Social engineering attacks pose a significant threat in the digital landscape, targeting individuals and organizations to manipulate and deceive them into performing harmful actions or disclosing sensitive information. As a data analyst, the objective is to develop a comprehensive system that can effectively detect social engineering content and protect users from potential harm. This report presents an analysis of merged code that combines phishing detection and spam detection techniques using machine learning algorithms.

**2. Phishing Detection**
Phishing detection involves identifying malicious links that attempt to deceive users into revealing sensitive information. The code combines various libraries such as `lattice`, `caret`, `e1071`, `tm`, and `httr`. The key scientific steps involved in phishing detection are as follows:

- **Step 1: Data Loading and Preparation**:
The phishing dataset is loaded from a CSV file using the `read.csv` function. Relevant columns are selected for analysis, including `length_url`, `ip`, `length_hostname`, and various other features. The data is cleaned by removing missing values using `na.omit` and removing duplicate entries using `distinct`. The `status` column, indicating phishing or legitimate URLs, is converted to binary labels with 1 representing phishing and 0 representing legitimate URLs.

```R
# Loading the phishing dataset
phishing_data <- read.csv("dataset_phishing.csv")

# Selecting the desired columns for phishing detection
selected_columns <- phishing_data %>%
  select(
    length_url, ip, length_hostname, nb_dots, nb_hyphens, nb_slash, nb_www, ratio_digits_url, http_in_path, https_token,
    submit_email, popup_window, whois_registered_domain, domain_registration_length, dns_record, google_index,
    ratio_intRedirection, ratio_extRedirection, ratio_intErrors, ratio_extErrors, phish_hints, nb_redirection,
    nb_external_redirection, punycode, status, nb_qm, nb_and, nb_or, nb_eq, nb_underscore, nb_tilde, nb_percent,
    nb_star, nb_colon, nb_comma, nb_semicolumn, nb_dollar
  )

# Clean the selected data
cleaned_data <- selected_columns %>%
  na.omit() %>%
  distinct()
cleaned_data$status <- ifelse(cleaned_data$status == "phishing", 1, 0)
```

- **Step 2: Model Training and Evaluation**:
The code utilizes Support Vector Machines (SVM) with a linear kernel for training the phishing detection model. The data is split into training and testing sets using `createDataPartition` from the `caret` library. The SVM model is trained on the training data using the `svm` function from the `e1071` library. Predictions are made on the test data, and the model's performance is evaluated using metrics such as accuracy and confusion matrix.

```R
# Split the data into training and testing sets
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
```

- **Step 3: Model Comparison and Saving**:
The code checks if a previously saved model exists. If it does, the accuracy of the current model is compared with the previous model. If the current model is more accurate, it is saved. Otherwise, the existing model remains the best. If no previous model exists, the current model is saved.

```R
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
```

**3. Spam Detection**
Spam detection involves identifying unsolicited and unwanted emails. The code utilizes libraries such as `tm`, `tidyverse`, `e1071`, and `SparseM` for spam detection. The scientific steps in spam detection are as follows:

- **Step 1: Data Loading and Preparation**:
The spam dataset is loaded from CSV files using the `read_csv` function. The relevant columns are selected, including the email body (`Body`) and the label (`Label`). Data cleaning techniques are applied to remove empty or "empty" values in the `Body` column and filter out labels other than 0 or 1. Duplicate entries based on the `Body` column are removed to ensure unique data points.

```R
# Read the CSV files
spamassasin <- read_csv("completeSpamAssassin.csv")
spamsubset <- read_csv("enronSpamSubset.csv")
lingspam <- read_csv("lingSpam.csv")

# Combine the datasets
spamdata <- bind_rows(spamassasin, spamsubset, lingspam) %>%
  select(Body, Label)

# Remove rows with empty or "empty" values in the 'Body' column
spamdata <- spamdata %>% filter(Body != "" & Body != "empty")

# Remove rows with Label values other than 0 or 1
spamdata <- spamdata %>% filter(Label %in% c(0, 1))

# Remove duplicates based on the 'Body' column
spamdata <- spamdata %>% distinct(Body, .keep_all = TRUE)
```

- **Step 2: Text Preprocessing and Feature Extraction**:
The code performs text preprocessing on the email bodies using techniques such as converting text to lowercase, removing punctuation, numbers, and stopwords, and stripping whitespace. The processed text is converted into a document-term matrix using the `DocumentTermMatrix` function from the `tm` library. The data is split into training and testing sets using `createDataPartition` from the `caret` library.

```R
# Convert the 'Body' column to a corpus
corpus <- Corpus(VectorSource(spamdata$Body))

# Perform text preprocessing
corpus <- corpus %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Create a document-term matrix
dtm <-

 DocumentTermMatrix(corpus)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(spamdata$Label, p = 0.8, list = FALSE)
train_data <- dtm[train_index, ]
test_data <- dtm[-train_index, ]
train_labels <- spamdata$Label[train_index]
test_labels <- spamdata$Label[-train_index]
```

- **Step 3: Model Training and Evaluation**:
The code trains a Support Vector Machine (SVM) model with a linear kernel using the training data. The SVM model is trained using the `svm` function from the `e1071` library. Predictions are made on the test data, and the model's performance is evaluated using metrics such as accuracy, precision, and confusion matrix.

```R
# Train the model using the training data
model <- svm(x = as.matrix(train_data), y = as.factor(train_labels), kernel = "linear", verbose = TRUE)

# Make predictions on the test data
predictions <- predict(model, newdata = as.matrix(test_data))

# Evaluate the model
confusion_matrix <- table(predictions, test_labels)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- caret::precision(predictions, test_labels)
```

- **Step 4: Model Comparison and Saving**:
Similar to the phishing detection step, the code checks if a previously saved model exists. If it does, the precision of the current model is compared with the previous model. If the current model's precision is better, the new model is saved. Otherwise, the existing model remains the best. If no previous model exists, the current model is saved.

```R
# Check if an existing model file exists
if (file.exists("spam.rds")) {
  existing_model <- readRDS("spam.rds")
  
  # Check if the current model's precision is better than the existing model
  if (precision > caret::precision(predict(existing_model, test_data), test_labels)) {
    # Save the new model
    saveRDS(model, "spam.rds")
    message("New model saved.")
  } else {
    message("Existing model remains the best.")
  }
} else {
  # Save the model as it's the first time
  saveRDS(model, "spam.rds")
  message("Model saved.")
}
```

**1. Confusion Matrix for Phishing Detection:**

```R
# Plotting the confusion matrix for phishing detection
library(ggplot2)

# Create a data frame from the confusion matrix
confusion_df <- as.data.frame.matrix(confusion_matrix)

# Generate a heatmap of the confusion matrix
ggplot(data = confusion_df, aes(x = Col1, y = rownames(confusion_df), fill = as.factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("#FF0000", "#00FF00"), labels = c("Legitimate", "Phishing")) +
  labs(x = "Actual", y = "Predicted", fill = "Status") +
  theme_bw()
```

**2. Confusion Matrix for Spam Detection:**

```R
# Plotting the confusion matrix for spam detection
library(ggplot2)

# Create a data frame from the confusion matrix
confusion_df <- as.data.frame.matrix(confusion_matrix)

# Generate a heatmap of the confusion matrix
ggplot(data = confusion_df, aes(x = Col1, y = rownames(confusion_df), fill = as.factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("#FF0000", "#00FF00"), labels = c("Not Spam", "Spam")) +
  labs(x = "Actual", y = "Predicted", fill = "Status") +
  theme_bw()
```



**4. Social Engineering Detection**
The code includes a function called `social_engineering_detection` that takes a string as input and detects the presence of social engineering content. The function utilizes the trained phishing and spam detection models to make predictions and identify potential threats.

```R
# Social Engineering Detection Function
social_engineering_detection <- function(teststr) {
  # Phishing Detection Code Snippet
  # ...
  
  # Spam Detection Code Snippet
  # ...
}

# Test the function with a sample string
teststr <- "..."
if (social_engineering_detection(teststr)) {
  print("This is a harmful social engineering content, please avoid!")
} else {
  print("This is a safe content.")
}
```

**5. Conclusion**
In conclusion, this report presented a comprehensive analysis of merged code for social engineering detection using machine learning. The code covered phishing detection and spam detection techniques, including data loading and preparation, model training, evaluation, model comparison, and saving. The merged code provides a holistic approach to identify potential social engineering threats and protect users from malicious content. Further improvements and refinements can be made to enhance the accuracy and efficiency of the detection system.
