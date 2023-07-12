library(tm)
library(tidyverse)
library(e1071)
library(SparseM)
library(caret)

# Load the saved model
saved_model <- readRDS("spam.rds")

preprocess_text <- function(text) {
  corpus <- Corpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  return(corpus)
}


example_bodies <- c(
  "Subject: email 57 million people for $ 99 57 million",
  "Subject: do want the best and economical hunting vacation of your life ? if you want the best hunting and camping vacation of your life , come to felton 's hunting camp in wild and wonderful west virginia . $ 50 . 00 per day pays for your room and three home cooked meals ( packed lunch if you want to stay out in the woods at noon ) with cozy accomodations . reserve your space now . following seasons are now being booked for 1998 : buck season - nov . 23 - dec . 5 doe season - to be announced ( please call ) muzzel loader ( deer ) - dec . 14 - dec . 19 archery ( deer ) - oct . 17 - dec . 31 turkey sesson - oct . 24 - nov . 14 e - mail us at 110734 . 2622 @ compuserve . com",
  "Subject: the best ,just got better the 2 newest and hottest interactive adult web sites will soon be the largest ! ! ! check out both of the following adult web sites free samples and trial memberships ! ! ! ! live triple x adult entertainment and pictures . new content added weekly ! ! ! girls , boys , couples , and more ! ! ! check them both out : http : / / www2 . dirtyonline . com http : / / www . chixchat . com"
)

preprocessed_bodies <- preprocess_text(example_bodies)

# Create a document-term matrix
dtm <- DocumentTermMatrix(preprocessed_bodies)
dtm <- as.matrix(dtm)

predictions <- predict(saved_model, new_data=dtm)

for (i in 1:length(example_bodies)) {
  if (predictions[i] == 1) {
    print(paste("Email", i, "is predicted to be spam (label = 1)."))
  } else {
    print(paste("Email", i, "is predicted not to be spam (label = 0)."))
  }
}

