library(tm)
library(wordcloud)
library(memoise)
library(pacman)
library(XML)
library(dplyr)
library(stringr)
library(rvest)
library(audio)
library(sentimentr)
library(stringi)

amazon_scraper <- function(doc, reviewer = T, delay = 0){
  
  #if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  #pacman::p_load_gh("trinker/sentimentr")
  #pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  author <- doc %>%
    html_nodes(".review-byline .author") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  helpful <- doc %>%
    html_nodes(".cr-vote-buttons .a-color-secondary") %>%
    html_text() %>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric()
  
  if(reviewer == T){
    
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])
      ) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()
      ) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>%
          trim()
      ) %>% as.numeric()
    
    df <- data.frame(title, date, ver.purchase, format, stars, comments,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
    
  } else df <- data.frame(title, author, date, ver.purchase, format, stars, comments, stringsAsFactors = F)
  
  return(df)
}

# getTermMatrix <- memoise(function(books) {
#   # Careful not to let just any name slip in here; a
#   # malicious user could manipulate this value.
#   #books=reviews_all$comments
# 
#   text <- readLines(sprintf("./%s.txt.gz", books),
#                    encoding="UTF-8")
# 
#   #text = books
#   text$comments=str_replace_all(books,"[^[:graph:]]", " ")
# 
#   #text = books
#    text=str_replace_all(text,"[^[:graph:]]", " ")
#    myCorpus = Corpus(VectorSource(text))
#    myCorpus = tm_map(myCorpus, content_transformer(tolower))
#    myCorpus = tm_map(myCorpus, removePunctuation)
#    myCorpus = tm_map(myCorpus, removeNumbers)
#    myCorpus = tm_map(myCorpus, removeWords,
#                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
#   
#    myCorpus = tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte"))
#   
#    myDTM = TermDocumentMatrix(myCorpus,
#                               control = list(minWordLength = 1))
#   
#    m = as.matrix(myDTM)
#   
#    sort(rowSums(m), decreasing = TRUE)
#   
#   
#   text = books
#   
#   text=str_replace_all(text,"[^[:graph:]]", " ") 
#   
#   docs <- Corpus(VectorSource(text))
#   toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#   docs <- tm_map(docs, toSpace, "/")
#   docs <- tm_map(docs, toSpace, "@")
#   docs <- tm_map(docs, toSpace, "\\|")
#   
#   # Remove numbers
#   docs <- tm_map(docs, removeNumbers)
#   # Remove english common stopwords
#   docs <- tm_map(docs, removeWords, stopwords("english"))
#   # Remove your own stop word
#   # specify your stopwords as a character vector
#   docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
#   # Remove punctuations
#   docs <- tm_map(docs, removePunctuation)
#   # Eliminate extra white spaces
#   docs <- tm_map(docs, stripWhitespace)
#   # Text stemming
#   #docs <- tm_map(docs, stemDocument)
#   
#   docs = tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
#   
#   dtm <- TermDocumentMatrix(docs)
#   m <- as.matrix(dtm)
#   sort(rowSums(m), decreasing = TRUE)
#   
#   v = m
#   wordcloud(words = names(v), freq = v, min.freq = 1,
#   max.words=100, random.order=FALSE, rot.per=0.35,
#   colors=brewer.pal(8, "Dark2"))
#   
#   # v <- sort(rowSums(m),decreasing=TRUE)
#   # d <- data.frame(word = names(v),freq=v)
#   # head(d, 10)
#   # 
#   # set.seed(1234)
#   # wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#   #           max.words=100, random.order=FALSE, rot.per=0.35, 
#   #           colors=brewer.pal(8, "Dark2"))
#   
# })


getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  
  #text <- readLines(sprintf("./%s.txt.gz", book), encoding="UTF-8")
  
  #text <- stri_encode(book, "", "UTF-8")
  
  #text=str_replace_all(book,"[^[:graph:]]", " ") 
  
  #text = stri_trans_tolower(book)
  text <- sapply(book, function(x) iconv(enc2utf8(x), sub = "byte"))
  
  #text = book
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  myCorpus = tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  d
})
