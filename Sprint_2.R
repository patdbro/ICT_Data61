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



setwd("~/ICT_procurement_research/Sprint_2")

ict.final <- read.csv("ict_final.csv", stringsAsFactors = FALSE)
ict.final <- ict.final[,-1]

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
plot(ict_4, vertex.size = 4, vertex.label = NA, edge.arrow.size = 0.25, layout = layout.kamada.kawai, main = "Cluster 3 vendors - Business Service Contracts (cluster 124)")

