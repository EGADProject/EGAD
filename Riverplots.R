library (riverplot)
library (plyr)
library (magrittr)
library (xlsx)
library (networkD3)

setwd("~/Desktop")

nodes <- read.xlsx("river.xlsx","nodes")
edges <- read.xlsx("river.xlsx","edges")

d3.nodes <- read.xlsx("river.xlsx","d3nodes")
d3.edges <- read.xlsx("river.xlsx","d3edges")


nodes$ID <- as.character(nodes$ID)
nodes$Label <- as.character(nodes$Label)
d3.nodes$Label <- as.character(d3.nodes$Label)

edges$N1 <- as.character(edges$N1)
edges$N2 <- as.character(edges$N2)

sankeyNetwork(Links = d3.edges,Nodes = d3.nodes,Source = "Source",Target = "Target", Value = "Value",NodeID = "Label", fontsize=14)

style <- list()

river<-makeRiver(nodes,edges,node_labels = nodes$Label) %>% 
  plot(.,srt=0)
  
              
              
              
              
              
              
              
              
              
              
              
              