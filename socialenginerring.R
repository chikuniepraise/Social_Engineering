social_engineering_detection <- function(teststr) {
  library(stringr)
  library(httr)
  library(tm)
  library(tidyverse)
  library(e1071)
  library(SparseM)
  library(caret)
  
  # Load the saved models
  spammodel <- readRDS("spam.rds")
  phishingmodel <- readRDS("phishing.rbs")
  
  has_ascii_characters <- function(str) {
    encoding <- Encoding(str)
    return(encoding == "unknown" || encoding == "ASCII")
  }
  
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
    
    # length_url feature
    link_feature$length_url <- nchar(link)
    hostname <- strsplit(gsub("http://|https://|www\\.", "", link), "/")[[c(1, 1)]]
    link_feature$length_hostname <- nchar(hostname)
    is_punny=0
    if (has_ascii_characters(hostname)) {
      is_punny=1
    }
    link_feature$punycode <- is_punny
    link_feature$nb_dots <- length(str_locate_all(hostname, "\\.")[[1]])
    link_feature$nb_hyphens <- length(str_locate_all(hostname, "-")[[1]])
    link_feature$nb_qm <- str_count(hostname, "\\?")
    link_feature$nb_and <- str_count(hostname, "&")
    link_feature$ip <- 1
    link_feature$nb_slash <- str_count(hostname, "\\/")
    link_feature$nb_www <- str_count(hostname, "www")
    link_feature$nb_or <- str_count(hostname, "\\|")
    link_feature$nb_eq <- str_count(hostname, "=")
    link_feature$nb_underscore <- str_count(hostname, "_")
    link_feature$nb_tilde <- str_count(hostname, "~")
    link_feature$nb_percent <- str_count(hostname, "%")
    link_feature$nb_star <- str_count(hostname, "\\*")
    link_feature$nb_colon <- str_count(hostname, ":")
    link_feature$nb_comma <- str_count(hostname, ",")
    link_feature$nb_semicolon <- str_count(hostname, ";")
    link_feature$nb_dollar <- str_count(hostname, "\\$")
    
    ratio_digits_url <- sum(str_count(link, "\\d")) / nchar(link)
    link_feature$ratio_digits_url <- ratio_digits_url
    http_in_path <- grepl("http", link)
    link_feature$http_in_path <- as.numeric(http_in_path)
    https_token <- grepl("https", link, ignore.case = TRUE)
    link_feature$https_token <- as.numeric(https_token)
    submit_email <- grepl("submit_email", link, ignore.case = TRUE)
    link_feature$submit_email <- as.numeric(submit_email)
    phish_hints <- grepl("phish|spoof|fake", link, ignore.case = TRUE)
    link_feature$phish_hints <- as.numeric(phish_hints)
    
    
    response <- GET(link)
    
    
    # Extract the number of external redirections
    nb_external_redirection <- length(response$url) - 1
    link_feature$nb_external_redirection <- nb_external_redirection
    link_feature$nb_redirection <- nb_external_redirection
    
    link_feature$ratio_intRedirection <- nb_external_redirection/length(response$url)
    link_feature$ratio_extRedirection <- nb_external_redirection/length(response$url)
    
    ratio_extErrors <- sum(str_count(response$content, "error")) / nchar(response$content)
    ratio_extErrors <- any(ratio_extErrors)
    link_feature$ratio_extErrors <- as.double(ratio_extErrors)
    link_feature$ratio_intErrors <- as.double(ratio_extErrors)
    
    popup_window <- any(grepl("pop|popup|window", response$content, ignore.case = TRUE))
    link_feature$popup_window <- as.double(popup_window)
    
    # Check if the URL has a DNS record
    link_feature$dns_record <- 1
    
    # Check if the URL is indexed by Google
    google_index <- grepl("site:google.com", link)
    link_feature$google_index <- as.numeric(google_index)
    
    # Get the domain from the hostname
    domain <- str_extract(hostname, "(?<=\\.)([^.]+\\.[^.]+)$")
    
    # Check if the domain is registered
    whois_result <- tryCatch(whois(domain), error = function(e) NULL)
    is_registered <- !is.null(whois_result)
    link_feature$whois_registered_domain <- as.numeric(is_registered)
    
    # Get the domain registration length
    if (is_registered) {
      reg_date <- whois_result$created
      reg_length <- as.numeric(difftime(Sys.Date(), reg_date, units = "days"))
    } else {
      reg_length <- 0
    }
    link_feature$domain_registration_length <- reg_length
    
    # Append the link features to the list
    link_data <- c(link_data, list(as.data.frame(t(link_feature), stringsAsFactors = FALSE)))
    df <-data.frame(t(sapply(link_feature,c)))
    new_predictions <- predict(phishingmodel$model,df)
    predlist[link] <- new_predictions
  }
  
  # Combine all the link data rows into a single data frame
  new_data <- do.call(rbind, link_data)
  
  example_bodies <- c(
    teststr
  )
  preprocessed_bodies <- preprocess_text(example_bodies)
  # Create a document-term matrix
  dtm <- DocumentTermMatrix(preprocessed_bodies)
  dtm <- as.matrix(dtm)
  predictions <- predict(spammodel, new_data=dtm)
  predlist['spam']<-predictions[1]
  
  # Convert the list of link features to a data frame
  # view(new_data)
  # print(teststr)
  # view(predlist)
  attacked=FALSE
  
  for (key in names(predlist)) {
    if(predlist[key]>=1 && key != "spam"){
      return(TRUE)
    }
  }
  return(FALSE)
}

teststr <- "1) Fight The Risk of Cancer! http://www.adclick.ws/p.cfm?o=315&s=pk0072) Slim Down - Guaranteed to lose 10-12 lbs in 30 days http://www.adclick.ws/p.cfm?o=249&s=pk0073) Get the Child Support You Deserve - Free Legal Advice http://www.adclick.ws/p.cfm?o=245&s=pk0024) Join the Web's Fastest Growing Singles Community http://www.adclick.ws/p.cfm?o=259&s=pk0075) Start Your Private Photo Album Online! http://www.adclick.ws/p.cfm?o=283&s=pk007Have a Wonderful Day, Offer Manager PrizeMamaIf you wish to leave this list please use the link below. http://www.qves.com/trim/?zzzz@spamassassin.taint.org%7C17%7C308417"
#teststr <- 'Subject: vilem mathesius lecture series 13 - prague , nov . 98    the vilem mathesius centre for research and education in semiotics and linguistics presents the vilem mathesius lecture series 13 november 9 - - 20 , 1998 prague , czech republic call for participation & call for grant applications the thirteenth cycle of the vilem mathesius lecture series , organized by the vilem mathesius centre for research and education in semiotics and linguistics ( charles university ) , will be held in prague , czech republic , from november 9 until 20 , 1998 . the scientific program will consist of the following invited courses ( usually three 90 minutes lectures ) : * emmon bach ( canada ) : " problems of universal and parochial grammar " * joan bresnan ( usa ) : " optimal syntax " * nicoletta calzolari ( italy ) : " corpus based lexicon building " * bernard comrie ( germany ) : " ( 1 ) advances in our understanding of relative clauses . ( 2 ) form and function in reference-tracking systems . ( 3 ) agreement in tsez ( ne caucasian ) : a typological assessment . " * edward l . keenan ( usa ) : < tba > * christian lehmann ( germany ) : " typology of possession " * karen sparck - jones ( england ) : " information retrieval and language processing " * hans uszkoreit ( germany ) : " modelling of linguistic performance " * bonnie webber ( usa ) : < tba > we are still waiting for confirmation from m . a . k . halliday , and h - j . lieb . among the czech lecturers invited to vmc 13 are frantisek cermak , miroslav cervenka , jan firbas , jan hajic , eva hajicova , jaroslav peregrin , and petr sgall . the exact time schedule will be announced later . grants ( deadline : may 31 , 1998 ) a limited number of grants is available for students from post-communist countries . the deadline for application is * may 31 , 1998 * . ideally , an application for a grant should include a letter of motivation and a letter of recommendation by a supervisor . applications should be send to professor eva hajicova at the address below . applicants will be notified by the end of june regarding approval of their application . participation ( deadline : september 15 , 1998 ) the participation fee for vmc 13 is usd 350 , which includes tuition fee , accommodation , and lunches . in order to ensure accommodation , ( paying ) participants should register before * september 15 , 1998 * . please contact mrs . brdickova or prof . hajicova at the following address for registration or further information . mrs . libuse brdickova institute of formal and applied linguistics ufal mff uk malostranske nam . 25 cz-11800 praha 1 czech republic { hajicova , brdickov } @ ufal . mff . cuni . cz ( phone ) + + 420 - 2-2191 - 4278 ( fax ) + + 420 - 2-2191 - 4309 check our website at http : / / kwetal . ms . mff . cuni . cz / ~ gj / vmc / . - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - geert - jan m . kruijff institute of formal & applied linguistics / linguistic data laboratory faculty of mathematics and physics , charles university malostranske nam . 25 , cz-118 00 prague 1 , czech republic phone : + + 420 - 2-2191 - 4255 fax : + + 420 - 2-2191 - 4309 email : gj @ ufal . ms . mff . cuni . cz , gj @ acm . org www : http : / / kwetal . ms . mff . cuni . cz / ~ gj /'

if(social_engineering_detection(teststr)){
  print("This is a harmfull social engineering content, please avoid!")
}else{
  print("this is a safe content")
}
