# install.packages(c("tm", "tidyverse", "e1071", "SparseM"))

library(tm)
library(tidyverse)
library(e1071)
library(SparseM)
library(caret)

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
dtm <- DocumentTermMatrix(corpus)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(spamdata$Label, p = 0.8, list = FALSE)
train_data <- dtm[train_index, ]
test_data <- dtm[-train_index, ]
train_labels <- spamdata$Label[train_index]
test_labels <- spamdata$Label[-train_index]

# Train the model using the training data
model <- svm(x = as.matrix(train_data), y = as.factor(train_labels), kernel = "linear", verbose = TRUE)

# Make predictions on the test data
predictions <- predict(model, newdata = as.matrix(test_data))

# Evaluate the model
confusion_matrix <- table(predictions, test_labels)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Calculate precision
precision <- caret::precision(predictions, test_labels)

# Print the confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(predictions);

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

