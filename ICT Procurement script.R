library(tidyverse)
library(stringr)
library(tm)
library(XML)
library(xml2)
library(RCurl)
library(RJSONIO)
library(jsonlite)
library(igraph)
library(GGally)

setwd("~/ICT_procurement_research/")

ict.all <- read.csv("ict_all.csv", stringsAsFactors = FALSE)

json.df = data.frame(matrix(nrow = 0, ncol=10), stringsAsFactors = FALSE)
colnames(json.df) <- json.df.names

for(i in 1:nrow(ABN.co)) {
  url.abn <- paste0("https://abr.business.gov.au/json/AbnDetails.aspx?abn=",ABN.count[i,1], "&callback=callback&guid=b4d042f8-5440-4f80-ae26-ecb487875a22", sep = "")
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

filter(ABN == "ABN Exempt")

  abn.exempt$Supplier.Name[11306] <- "Uauline Presta de Internet Ltda"
  abn.exempt$Supplier.Name[18781] <- "IFREMER Institut Franais de Reche"
  abn.exempt$Supplier.Name[15321] <- "FACTIVA DOW JONES & REUTERS"
  abn.exempt$Supplier.Name[15320] <- "FACTIVA DOW JONES & REUTERS"
  abn.exempt$Supplier.Name[15319] <- "FACTIVA DOW JONES & REUTERS"
  abn.exempt$Supplier.Name[15199] <- "Funda Anglo Brasileira de Educa e Cultura de Sao Paulo"
  abn.exempt$Supplier.Name[15158] <- "Funda Anglo Brasileira de Educa e Cultura de Sao Paulo"
  abn.exempt$Supplier.Name[11306] <- "Uauline Presta de Internet Ltda"
  abn.exempt$Supplier.Name <- tolower(abn.exempt$Supplier.Name)

ict.comb.ae <- ict.comb
ict.comb.ae[ict.comb.ae$ABN == "ABN Exempt",] <- abn.exempt
ict.comb.ae$Supplier.Name <- str_replace_all(ict.comb.ae$Supplier.Name, "\\\b86\\\ff", "")
ict.comb.ae$Supplier.Name <- tolower(ict.comb.ae$Supplier.Name)
ict.comb.ae$Supplier.Name <- trimws(ict.comb.ae, which = "right")

ict.net <- data.frame(ict.comb.ae$Agency, ict.comb.ae$Supplier.Name, stringsAsFactors = FALSE)
colnames(ict.net) <- c("Agency", "Supplier")

ict.igraph <- graph_from_data_frame(ict.igraph)
ict.simp <- simplify(ict.igraph)
jpeg("ict_simp.jpg", width = 1440, height = 960, quality = 300)
plot(ict.simp, vertex.label = NA, vertex.size = 3, edge.color = "grey", edge.alpha = .3)
dev.off()

cleanse <- function(x) {
  cat.dtm <- Corpus(VectorSource(x))
  cat.dtm <- tm_map(cat.dtm, content_transformer(tolower))
  cat.dtm <- tm_map(cat.dtm, removePunctuation)
  cat.dtm <- tm_map(cat.dtm, removeNumbers)
  cat.dtm <- tm_map(cat.dtm, removeWords, stopwords("english"))
  cat.dtm <- tm_map(cat.dtm, stemDocument)
  cat.dtm <- tm_map(cat.dtm, stripWhitespace)
  cat.dtm <- DocumentTermMatrix(cat.dtm)
}

cat.2.sparse <- removeSparseTerms(cat.2, .99964)

kfit100.sp <- kmeans(dist(as.matrix(cat.2.sparse)), centers = 100, nstart = 100)
cat.df <- cbind(cat.df, kfit100.sp$cluster)

kfit150.sp <- kmeans(dist(as.matrix(cat.2.sparse)), centers = 150, nstart = 100)
kfit150 <- kmeans(dist(as.matrix(cat.2)), centers = 150, nstart = 100)
kfit250.sp <- kmeans(dist(as.matrix(cat.2.sparse)), centers = 250, nstart = 100)
kfit250 <- kmeans(dist(as.matrix(cat.2)), centers = 250, nstart = 100)

cat.df <- cbind(cat.df, kfit150.sp$cluster,kfit150$cluster, kfit250$cluster, kfit250.sp$cluster)
write.csv(cat.df, "category_clusters.csv")

cge <- unique(ict.comb.ae$EntityName[ict.comb.ae$EntityTypeCode == "CGE"])
cge.agency <- unique(ict.comb.ae$Agency)
