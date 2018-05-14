library(tidyverse)
library(stringr)
library(tm)
library(XML)
library(xml2)
library(RCurl)
library(RJSONIO)
library(jsonlite)
setwd("~/Documents/ICT procurement")

ict.all <- read.csv("ict_all.csv", stringsAsFactors = FALSE)

json.df = data.frame(matrix(nrow = 0, ncol=10), stringsAsFactors = FALSE)

json.df.names <- c("Abn", "AbnStatus", "AddressDate", "AddressPostcode", "AddressState", "EntityName", "EntityTypeCode", "EntityTypeName", "Gst", "Message")
colnames(json.df) <- json.df.names

for(i in 1:nrow(ABN.co)) {
  url.abn <- paste0("https://abr.business.gov.au/json/AbnDetails.aspx?abn=",ABN.co[i,1], "&callback=callback&guid=b4d042f8-5440-4f80-ae26-ecb487875a22", sep = "")
  json.data <- getURL(url.abn)
  json.data <- str_replace_all(json.data, "callback\\(", "")
  json.data <- str_replace_all(json.data, "\\)", "")
  json.list <- jsonlite::fromJSON(json.data)
  json.list <- lapply(json.list, function(x) ifelse(is.null(x), "", x))
  json.col <- ldply(json.list, data.frame, stringsAsFactors = FALSE)
  json.col <- filter(json.col, .id !="BusinessName")
  json.t <- t(json.col[,2])
  colnames(json.t) <- json.df.names
  json.df <- rbind(json.df, json.t)
  json.df
}

json.l <- list()
for(i in 1:nrow(ABN.co)) {
  url.abn <- paste0("https://abr.business.gov.au/json/AbnDetails.aspx?abn=",ABN.co[i,1], "&callback=callback&guid=b4d042f8-5440-4f80-ae26-ecb487875a22", sep = "")
  json.data <- getURL(url.abn)
  json.data <- str_replace_all(json.data, "callback\\(", "")
  json.data <- str_replace_all(json.data, "\\)", "")
  json.list <- jsonlite::fromJSON(json.data)
  json.list <- json.list[1:9]
  json.l <- append(json.l, json.list)
  json.l
}

colnames(ict.all)[5] <- "ABN"
colnames(json.df)[1] <- "ABN"
ict.comb <- left_join(ict.all, json.df, by = "ABN")

colnames(ict.comb)[10] <- "Value"
ict.comb$EntityTypeCode <- as.factor(ict.comb$EntityTypeCode)
ict.comb$AbnStatus <- as.factor(ict.comb$AbnStatus)

cal.all <- unique(ict.comb$Category)
cat.dtm <- Corpus(VectorSource(cat.all))
cat.dtm <- tm_map(cat.dtm, content_transformer(tolower))
cat.dtm <- tm_map(cat.dtm, removePunctuation)
cat.dtm <- tm_map(cat.dtm, removeNumbers)
cat.dtm <- tm_map(cat.dtm, removeWords, stopwords("english"))
cat.dtm <- tm_map(cat.dtm, stripWhitespace)
cat.dtm <- DocumentTermMatrix(cat.dtm)
cat.m <- as.matrix(cat.dtm)
cat.d <- dist(cat.m)

kfit25 <- kmeans(cat.d, k = 25, nstart = 100)
kfit50 <- kmeans(cat.d, k = 50, nstart = 100)
kfit100 <- kmeans(cat.d, k = 100, nstart = 100)

cat.df <- data.frame(Category = cat.all, kf25 = kfit25$cluster, kf50 = kfit50$cluster, kf100 = kfit100$cluster)

ict.net <- data.frame(ict.comb$Agency, ict.comb$EntityName, ict.comb$Category, ict.comb$Value, stringsAsFactors = FALSE)

