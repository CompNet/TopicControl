# setwd("D:/Eclipse/workspaces/Networks")
# source("TopicControl/src/properties.R")

library("igraph")


# set up net folders
data.folder <- "data/"
folders <- c(
		"CKM_Physicians_Innovation/",
		"CS_Aarhus/",
#		"EU_Air_Transport/", # 37 link types, that's too many for now
		"Kapferer_Tailor_Shop/",
		"Krackhardt8-High_Tech/",
		"Lazega_Law_Firm/",
		"London_Transport/",
		"Padgett_Florence_Families/",
		"Vickers_Chan_7thGraders/"
)
f <- length(folders)

# init property table
prop <- data.frame(
	row.names=folders, 
	Nodes=rep(0,f),
	Links=rep(0,f),
#	LinkTypes=rep(0,k),
	stringsAsFactors=FALSE
)

# process properties
for(folder in folders)
{	# init file names
	net.folder <- paste(data.folder, folder, sep="")
	net.file <- paste(net.folder, "network.graphml", sep="")
	
	# load graph
	g <- read.graph(file=net.file,format="graphml")
	
	# add to properties
	prop[folder,"Nodes"] <- vcount(g)
	prop[folder,"Links"] <- ecount(g)
	prop[folder,"LinkTypes"] <- length(unique(E(g)$type))
	types <- max(E(g)$type)
	for(t in 1:types)
	{	g2 <- subgraph.edges(graph=g, eids=which(E(g)$type==t), delete.vertices=FALSE)
		prop[folder,paste("Links",t,sep="")] <- ecount(g2)
		prop[folder,paste("Density",t,sep="")] <- graph.density(g2)
		prop[folder,paste("AverageDistance",t,sep="")] <- average.path.length(graph=g2, directed=TRUE, unconnected=TRUE)
	}
}

# record properties
prop.file <- paste(data.folder, "properties.txt", sep="")
write.table(x=prop, file=prop.file)
