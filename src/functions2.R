# Cette fonction calcule l'équilibre de notre système. Les inputs sont la matrice d'adjacence E (dimension I^2),
#la matrice des flows externes (dimension: IxC), et enfin le vecteur d'activation (dimension: I) 

equilibre<- function(E,lambda,alpha,npar=TRUE,print=TRUE) {
  Lambda<-array(0,c(dim(lambda)[1]))
  for(i in 1:dim(lambda)[1]){
    Lambda[i]<-sum(lambda[i,])+alpha[i]
  }
  lambda<-lambda*(1/Lambda) # creation de \frac{\lambda_{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  alpha<-alpha*(1/Lambda)  # creation de \frac{\alpha{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  for(c in 1:dim(lambda)[2]){
    E[,,c]<-E[,,c]*alpha
  }  
  dI<-dim(E)[1]
  dC<-dim(lambda)[2]
  x<-array(0,c(dI,dC))
  # calcul de l'équilibre
  for(c in 1:dC){
    x[,c]<-solve(E[,,c]-diag(dI),-lambda[,c])
  }
  return(x)
} 

#Cette function calcule lambda, sachant un k donnée. k est une matrice de dimension IxC.
control<- function(E,k,alpha,npar=TRUE,print=TRUE){
  dI<-dim(k)[1]
  dC<-dim(k)[2]
  lambda<-array(0,c(dI,dC))
  G<-(E%*%k)*alpha
  H<-E%*%k
  Lambda<-(sum(k)*sum(alpha)-sum(G))/(1-sum(k))
  for(i in 1:dI){
    for(c in 1:dC){
      lambda[i,c]<-k[i,c]*Lambda+k[i,c]*sum(alpha)-alpha[i]*(H[i,c])
    }
  }
  return(lambda)
}

simulation<- function(E,lambda,alpha,t,npar=TRUE,print=TRUE) {
  Lambda<-array(0,c(dim(lambda)[1]))
  for(i in 1:dim(lambda)[1]){
    Lambda[i]<-sum(lambda[i,])+alpha[i]
	lambda[i,] <- lambda[i,]*(1/Lambda[i])
  }
print(dim(alpha))  
print(dim(Lambda))  
   # creation de \frac{\lambda_{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  alpha <- alpha*(1/Lambda)  # creation de \frac{\alpha{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  for(c in 1:dim(lambda)[2]){
#	  print(dim(E[,,c]))
#	  print(dim(alpha))
    E[,,c] <- E[,,c]*alpha #E[i,i,c]
  }
  dI <- dim(E)[1]
  dC <- dim(lambda)[2]
  x <- array(0,c(dI,dC,t))
  # calcul de l'équilibre
  for(t in 1:(t-1)){
  for(c in 1:dC){
      x[,c,t+1] <- (E[,,c]) %*% x[,c,t] +lambda[,c]
    }   
  }
 
  return(x)
}

#####################
## Generates a sample following a power-law distribution
## ref: http://mathworld.wolfram.com/RandomNumber.html
## 		n: sample size
##		expnt: power law exponent (aka. alpha)
##		xmin: lower bound (default: 1)
##		xmax: upper bound (default: n)
##
#####################
rpower <- function(n=1000,expnt=3,xmin=1,xmax=1000)
{	result <- ((xmax^(-expnt+1)-xmin^(-expnt+1))*runif(n)+xmin^(-expnt+1))^(1/(-expnt+1));
	return(result)
}
