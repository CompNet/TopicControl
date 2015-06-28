# setwd("D:/Eclipse/workspaces/Networks/")
# source("properties.R")

library("igraph")


data.folder <- "TopicControl/data/"
# layerID nodeID nodeID weight=1
net.folder <- paste(data.folder,"Lazega_Law_Firm/",sep="")
file.in <- paste(net.folder,"Lazega-Law-Firm_multiplex.edges",sep="")
file.out <- paste(net.folder,"network.graphml",sep="")

directed <- TRUE

# read as a table
tab <- as.matrix(read.table(file.in))

# build igraph
edges <- as.vector(t(tab[,2:3]))
if(min(edges)==0)
	edges <- edges + 1
n <- max(edges)
types <- tab[,1]
g <- graph.empty(n=n,directed=directed)
g <- add.edges(g,edges,type=types)

# export as graphml
write.graph(graph=g,file=file.out,format="graphml")
