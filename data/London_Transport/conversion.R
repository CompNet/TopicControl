# setwd("D:/Eclipse/workspaces/Networks/")
# source("properties.R")

library("igraph")


data.folder <- "TopicControl/data/"
# layerID nodeID nodeID weight=1
net.folder <- paste(data.folder,"London_Transport/",sep="")
file.in <- paste(net.folder,"london_transport_multiplex.edges",sep="")
file.out <- paste(net.folder,"network.graphml",sep="")

directed <- FALSE

# read as a table
tab <- as.matrix(read.table(file.in))

# build igraph
edges <- as.vector(t(tab[,2:3]))
if(min(edges)==0)
	edges <- edges + 1
n <- max(edges)
types <- tab[,1]
weights <- tab[,4]
g <- graph.empty(n=n,directed=directed)
g <- add.edges(g,edges,type=types,weight=weights)

# export as graphml
write.graph(graph=g,file=file.out,format="graphml")
