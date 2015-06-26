# setwd("D:/Eclipse/workspaces/Networks/TopicControl/")
# source("properties.R")

library("igraph")


data.folder <- "data/"
# layerID nodeID nodeID weight=1
net.folder <- paste(data.folder,"Padgett_Florence_Families/",sep="")
file.in <- paste(net.folder,"Padgett-Florentine-Families_multiplex.edges",sep="")
file.out <- paste(net.folder,"network.graphml",sep="")

directed <- FALSE

# read as a table 
tab <- as.matrix(read.table(file.in))

# build igraph
edges <- as.vector(t(tab[,2:3]))
n <- max(edges)
types <- tab[,1]
g <- graph.empty(n=n,directed=directed)
g <- add.edges(g,edges,type=types)

# export as graphml
write.graph(graph=g,file=file.out,format="graphml")
