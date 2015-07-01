# setwd("D:/Eclipse/workspaces/Networks")
# source("TopicControl/src/properties.R")

library("igraph")


# set up net folders
data.folder <- "TopicControl/data/"
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
	cor.tab <- matrix(0,nrow=types,ncol=types)
	proba.tab <- matrix(0,nrow=types,ncol=types)
	for(t in 1:types)
	{	# get subnetwork and process its properties
		g2 <- subgraph.edges(graph=g, eids=which(E(g)$type==t), delete.vertices=FALSE)
		prop[folder,paste("Links",t,sep="")] <- ecount(g2)
		prop[folder,paste("Density",t,sep="")] <- graph.density(g2)
		prop[folder,paste("AverageDistance",t,sep="")] <- average.path.length(graph=g2, directed=TRUE, unconnected=TRUE)
		
		# process link type correlations
		if(is.weighted(g))
			m <- get.adjacency(graph=g2, type="both", attr="weight")
		else
			m <- get.adjacency(graph=g2, type="both")
		m <- as.vector(m)
		for(t2 in 1:types)
		{	g3 <- subgraph.edges(graph=g, eids=which(E(g)$type==t2), delete.vertices=FALSE)
			if(is.weighted(g))
				m2 <- get.adjacency(graph=g3, type="both", attr="weight")
			else
				m2 <- get.adjacency(graph=g3, type="both")
			m2 <- as.vector(m2)
			idx <- which(m!=0 | m2!=0) # ignore links absent from both graphs
			cor.tab[t,t2] <- cor(m[idx],m2[idx])
#			cor.tab[t,t2] <- cor(m,m2)
			proba.tab[t,t2] <- length(which(as.logical(m) & as.logical(m2)))/length(which(as.logical(m2)))
		}
	}
	
	# record type vs. type tables
	cor.file <- paste(net.folder, "correlations.txt", sep="")
	write.table(x=cor.tab, file=cor.file)
	proba.file <- paste(net.folder, "probas.txt", sep="")
	write.table(x=proba.tab, file=proba.file)
}

# record properties
prop.file <- paste(data.folder, "properties.txt", sep="")
write.table(x=prop, file=prop.file)
