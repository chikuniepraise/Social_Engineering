# Social Enginnering


**Data Collection and Preprocessing**

To train the spam detection model, we collected data from multiple sources and combined them into a single dataset. The following code snippet illustrates the data collection and preprocessing steps:

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
```

In this code, we read the CSV files containing spam data from different sources. We combined these datasets and selected the 'Body' and 'Label' columns. Rows with empty values or non-binary labels were filtered out. Additionally, duplicate rows based on the 'Body' column were removed. The 'Body' column was then converted into a corpus, and text preprocessing techniques such as converting to lowercase, removing punctuation and numbers, eliminating common English stopwords, and stripping whitespace were applied. Finally, a document-term matrix (dtm) was created to represent the text data numerically.

**4. Machine Learning Models**

To train the spam detection model, we used a Support Vector Machine (SVM) algorithm with a linear kernel. Here is the code snippet that demonstrates the training and evaluation process:

```R
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
print(predictions)
```

In this code, we split the data into training and testing sets using an 80-20 split ratio. The SVM model is trained on the training data using the `svm` function from the `e1071` package. We then make predictions on the test data and evaluate the model's performance using a confusion matrix and accuracy calculation. The precision is also calculated using the `caret::precision` function.

Feel free to include these code snippets in the respective sections of your report. Make sure to provide any additional explanations or insights to enhance the understanding of your work.
