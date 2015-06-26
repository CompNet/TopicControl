# setwd("D:/Eclipse/workspaces/Extraction")
# source("KeyPlayers/tests.R")

source("KeyPlayers/functions.R")

library("igraph")

# parameters
duration <- 20
nodes <- 10
topics <- 5
l.min <- 1
l.max <- 20
a.min <- 0
a.max <- 10

# init adjacency matrix
	adj <- array(0,dim=c(nodes,nodes,topics))
	# create graph, get matrix
	for(c in 1:topics)
	{	g <- interconnected.islands.game(islands.n=2, islands.size=nodes/2, islands.pin=0.05, n.inter=5)
#		g <- graph.full(n=nodes,directed=TRUE,loops=FALSE)
#		g <- erdos.renyi.game(n=nodes,p.or.m=0.05,directed=TRUE)
#		g <- barabasi.game(n=nodes,m=5,directed=TRUE)
		cat("density=",graph.density(g),"\n")
		cat("strongly connected=",is.connected(graph=g, mode="strong"),"\n")
		adj[,,c] <- as.matrix(get.adjacency(graph=g
			#,attr="weight"
		))
	}
	# normalize matrix
	adj <- adj / apply(X=adj,MARGIN=c(1), FUN=sum)
	adj[which(is.nan(adj))] <- 0
	
# init ppp intensities	
	# distribution over nodes
#	l <- runif(n=nodes, min=l.min, max=l.max)
#	l <- rpower(n=nodes, expnt=3, xmin=l.min, xmax=l.max)
	
	# distribution over topics
#	lambda <- t(sapply(
#		X=l, 
#		FUN=function(val) 
#			rnorm(n=topics,mean=val,sd=10)
#		))

	lambda <- matrix(data=l.max,nrow=nodes,ncol=topics)

# init imitation coefficient
#	alpha <- runif(n=nodes, min=a.min, max=a.max)
	alpha <- rep(a.max,nodes)
	
# process limit values
#eq <- equilibre(E=adj,lambda=lambda,alpha=alpha)
#print(eq)

# perform simultation
x <- simulation(E=adj, lambda=lambda, alpha=alpha, t=duration)

# plot results
data <- apply(x,c(3),function(m) apply(m,2,mean))
plot(x=1:duration, y=data[1,], type="n", ylim=c(0.017,0.019), xlim=c(0,5))#max(x,na.rm=TRUE))) 
#for(i in 1:topics)
#	lines(x=1:duration, y=data[i,], col=i)
#for(i in 1:nodes)
i=1
	lines(x=1:duration, y=x[i,2,], col=i)