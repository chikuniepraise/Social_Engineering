# Social Engineering

To incorporate the code for detecting social engineering content into your report, you can follow the same approach as before. Here's an example of how you can include the code snippets:

**Social Engineering Content Detection**

For detecting social engineering content, we developed a program that analyzes URLs and text content to identify potential malicious links and harmful social engineering tactics. The following code snippet demonstrates the detection process:

```R
# Load the saved models
spammodel <- readRDS("spam.rds")
phishingmodel <- readRDS("phishing.rbs")

# Define a function to check for ASCII characters
has_ascii_characters <- function(str) {
  encoding <- Encoding(str)
  return(encoding == "unknown" || encoding == "ASCII")
}

# Define a function to preprocess the text
preprocess_text <- function(text) {
  corpus <- Corpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  return(corpus)
}

# Extract links using regular expressions
links <- str_extract_all(teststr, "(?i)\\b(?:https?|ftp)://\\S+\\b")

# Initialize an empty list to store the extracted features
link_data <- list()
predlist <- c()

# Extract features for each link
for (link in links[[1]]) {
  # Initialize an empty list for storing the features of the current link
  link_feature <- list()
  
  # ... (code for extracting link features)

  # Append the link features to the list
  link_data <- c(link_data, list(as.data.frame(t(link_feature), stringsAsFactors = FALSE)))
  df <-data.frame(t(sapply(link_feature,c)))
  new_predictions <- predict(phishingmodel$model,df)
  predlist[link] <- new_predictions
}

# Combine all the link data rows into a single data frame
new_data <- do.call(rbind, link_data)

# Preprocess the text content
example_bodies <- c(teststr)
preprocessed_bodies <- preprocess_text(example_bodies)

# Create a document-term matrix
dtm <- DocumentTermMatrix(preprocessed_bodies)
dtm <- as.matrix(dtm)

# Make predictions using the saved models
predictions <- predict(spammodel, new_data=dtm)
predlist['spam'] <- predictions[1]

# Detect potential harmful links and social engineering content
attacked <- FALSE

for (key in names(predlist)) {
  if (predlist[key] >= 1 && key != "spam") {
    print(paste(key, "is harmful"))
    attacked <- TRUE
  }
}

# Check if the content is safe or potentially harmful
if (attacked == TRUE) {
  print("This is a harmful social engineering content, please avoid!")
} else {
  print("This is a safe content")
}
```

In this code, we load the saved models for spam detection (`spammodel`) and phishing detection (`phishingmodel`). We define helper functions to check for ASCII characters and preprocess the text content. The code then extracts links from the provided text and processes each link to extract features for analysis. The link features are stored in `link_data` and combined into a single data frame (`new_data`).

The text content is preprocessed using the `preprocess_text` function, and a document-term matrix (`dtm`) is created. Predictions are made for spam detection using the `spammodel`, and phishing detection predictions are made for each link using the `phishingmodel`. The predictions are stored in `predlist`.

The code then checks for potentially harmful links and social engineering content by iterating through `predlist`. If a potentially harmful link is detected, it is printed, and the `attacked` flag is set to `TRUE`. Finally, based on the value of `attacked`, the code prints whether the content is safe or potentially harmful.

Remember to include these code snippets in the appropriate section of your report, along with any additional explanations or insights you find relevant.
