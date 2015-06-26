# setwd("D:/Eclipse/workspaces/Networks")
# source("TopicControl/conversion.R")

library("igraph")


# set up net folders
data.folder <- "data/"
folders <- c(
		"CKM_Physicians_Innovation/",
		"CS_Aarhus/",
		"EU_Air_Transport/",
		"Kapferer_Tailor_Shop/",
		"Krackhardt8-High_Tech/",
		"Lazega_Law_Firm/",
		"London_Transport/",
		"Padgett_Florence_Families/",
		"Vickers_Chan_7thGraders/",
)
k <- length(folders)

# init property table
prop <- data.frame(
	Name=folders, 
	Nodes=rep(0,k),
	Links=rep(0,k),
	stringsAsFactors=FALSE
)

for(folder in folders)
{	# init file names
	net.folder <- paste(data.folder, folder, sep="")
	net.file <- paste(net.folder, "network.graphml", sep="")
	prop.file <- paste(net.folder, "properties.txt", sep="")
	
	# load graph
	g <- read.graph(file=net.file,format="graphml")
	prop <- data.frame()
	
	
}