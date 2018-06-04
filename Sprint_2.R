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
library("wordVectors", lib.loc="~/R/win-library/3.5")
library(tidytext)
library(widyr)
library(irlba)
library(broom)
library(lda)
library(topicmodels)
library(LDAvis)
library(cluster)
library(data.table)
library(xts)
library(statnet)
library(intergraph)
library(RSiena)
library(RXKCD)

setwd("~/ICT_procurement_research/Sprint_2")

ict.final <- read.csv("ict_final.csv", stringsAsFactors = FALSE)
ict.final <- ict.final[,-1]

##functions
rescale <- function(nchar, low, high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rescl <- ((high - low) * (nchar - min_d)/(max_d - min_d) + low)
  rescl
}

print_to_printer <- function(df, addquotes = FALSE)
{     
  write.csv(df, "~/ICT_procurement_research/Sprint_2/PRINT_ME.txt",quote = addquotes)
  shell("NOTEPAD /P C:\\Users\\dra067\\Documents\\ICT_procurement_research\\Sprint_2\\PRINT_ME.txt")   
  cat(quote(df), " has been printed to printer")
}


## Sample network
ict_5 <- ict.net %>%
   filter(Vendor.Category.Number == 5) %>%
   select(Agency, Supplier.Name)

ict_5 <- graph_from_data_frame(ict_5)
ict_5s <- simplify(ict_5)

plot(ict_5, vertex.label = NA, vertex.size = 7, layout = layout.kamada.kawai)
plot(ict_5s, vertex.label = NA, vertex.size = 7, layout = layout.kamada.kawai)

##add type attribute
gov_5 <- ict.net %>%
  filter(Vendor.Category.Number == 5) %>%
  select(Agency)
gov_5 <- unique(gov_5$Agency)

V(ict_5)$type <- ifelse(V(ict_5)$name %in% gov_5, "Government", "Private")
V(ict_5)$color <- ifelse(V(ict_5)$type == "Government", "lightblue", "salmon")
V(ict_5)$shape <- ifelse(V(ict_5)$type == "Government", "circle", "square")
plot(ict_5, vertex.label.cex = 0.8, vertex.label.color = "black")

#add contract cat number 
cat_5 <- ict.net %>%
  filter(Vendor.Category.Number == 5) %>%
  select(contract.category.number)
ict_5 <- set.edge.attribute(graph = ict_5, name = "category", value = cat_5)

##induce subgraph to specific edge category
ict_sub <-  subgraph.edges(ict_5, which(E(ict_5)[[1]]$category == 124))  ##business services category of contracts
plot(ict_sub, vertex.size = 4, vertex.label = NA, edge.arrow.size = 0.25, layout = layout.kamada.kawai, main = "Cluster 5 vendors - Business Service Contracts (cluster 124)")


## repeat for a new vendor cluster
ict_4 <- ict.net %>%
  filter(Vendor.Category.Number == 4) %>%
  select(Agency, Supplier.Name, contract.category.number) %>%
  filter(contract.category.number == 124) %>%
  select(Agency, Supplier.Name)

ict_4 <- graph_from_data_frame(ict_4)

##add type attribute
gov_4 <- ict.net %>%
  filter(Vendor.Category.Number == 4) %>%
  select(Agency)
gov_4 <- unique(gov_4$Agency)

V(ict_4)$type <- ifelse(V(ict_4)$name %in% gov_4, "Government", "Private")
V(ict_4)$color <- ifelse(V(ict_4)$type == "Government", "lightblue", "salmon")
V(ict_4)$shape <- ifelse(V(ict_4)$type == "Government", "circle", "square")


##induce subgraph to specific edge category
plot(ict_4, vertex.size = 4, vertex.label = NA, edge.arrow.size = 0.25, layout = layout.kamada.kawai, main = "Cluster 4 vendors - Business Service Contracts (cluster 124)")


##Cluster 4 demonstrates significant Scale Free characteristics

V(ict_4)[degree(ict_4) >7]$color <- "black"
node_size <- rescale(nchar = degree(ict_4), low = 3, high = 12)

plot(ict_4, vertex.label = NA, vertex.size = node_size, edge.arrow.size = .15, layout = layout_with_kk)


ict_subn <- asNetwork(ict_sub)

agency_category <- read.csv("PRINT_ME.csv")


## create example xts cross section of data
xts_4.124 <- ict.net %>%
  filter(Vendor.Category.Number == 4) %>%
  select(Agency, Supplier.Name, contract.category.number, Start.Date, Value) %>%
  filter(contract.category.number == 124) %>%
  select(Agency, Supplier.Name, Start.Date, Value)

xts_4.124 <- xts(xts_4.124[, -3], order.by = as.Date(xts_4.124[,3]))
xts_4.124.q1 <- xts_4.124["2007/200703"]
xts_4.124.q2 <- xts_4.124["200704/200706"]
xts_4.124.q3 <- xts_4.124["200707/200709"]
xts_4.124.q1 <- xts_4.124["200710/200712"]
xts_4.124.q5 <- xts_4.124["2008/200803"]
xts_4.124.q6 <- xts_4.124["200804/200806"]
xts_4.124.q7 <- xts_4.124["200807/200809"]
xts_4.124.q8 <- xts_4.124["200810/200812"]


q1.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q1[,c(1:2)]))
q2.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q2[,c(1:2)]))
q3.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q3[,c(1:2)]))
q4.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q4[,c(1:2)]))
q5.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q5[,c(1:2)]))
q6.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q6[,c(1:2)]))
q7.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q7[,c(1:2)]))
q8.ig <- graph_from_data_frame(as.data.frame(xts_4.124.q8[,c(1:2)]))

plot(q4.ig, vertex.size = 3, vertex.label.cex = .5, edge.arrow.size = .15, vertex.label = NA, main = "Cluster 4, Business Services Contracts - Q4 2007")
plot(q5.ig, vertex.size = 3, vertex.label.cex = .5, edge.arrow.size = .15, vertex.label = NA, main = "Cluster 4, Business Services Contracts - Q1 2008")
plot(q6.ig, vertex.size = 3, vertex.label.cex = .5, edge.arrow.size = .15, vertex.label = NA, main = "Cluster 4, Business Services Contracts - Q2 2008")
plot(q7.ig, vertex.size = 3, vertex.label.cex = .5, edge.arrow.size = .15, vertex.label = NA, main = "Cluster 4, Business Services Contracts - Q3 2008")
plot(q8.ig, vertex.size = 3, vertex.label.cex = .5, edge.arrow.size = .15, vertex.label = NA, main = "Cluster 4, Business Services Contracts - Q4 2008")




