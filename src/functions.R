require('magic')
require('limSolve')

# Cette fonction calcule l'équilibre de notre système. 
# Les inputs sont la matrice d'adjacence E (dimension I^2),
# la matrice des flots externes (dimension IxC), 
# et enfin le vecteur d'activation (dimension I) 
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

# Cette function calcule lambda, sachant un k donné. 
# k est une matrice de dimension IxC.
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

simulation<- function(E,lambda,alpha,t,theta,npar=TRUE,print=TRUE) {
  Lambda<-array(0,c(dim(lambda)[1]))
  for(i in 1:dim(lambda)[1]){
    Lambda[i]<-sum(lambda[i,])+alpha[i]
    lambda<-lambda*(1/Lambda) 
  }
 # creation de \frac{\lambda_{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  alpha<-alpha*(1/Lambda)  # creation de \frac{\alpha{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  for(c in 1:dim(lambda)[2]){
    E[,,c]<-E[,,c]*alpha
  }
  dI<-dim(E)[1]
  dC<-dim(lambda)[2]
  x<-array(0,c(dI,dC,t))
  # calcul de l'équilibre
  for(t in 1:(t-1)){
  for(c in 1:dC){
      x[,c,t+1]<-(E[,,c])%*%(x[,c,t])^theta +lambda[,c]
    }   
  }
 
  return(x)
}

# dim k = IxC, la somme des lignes doit être inférieure à 1
# dim E = IxIxC
# alpha = contrainte scalaire
# beta = contrainte scalaire
Control<-function(E,k,alpha,beta,npar=TRUE,print=TRUE){
	dI<-dim(E)[1]
	dC<-dim(k)[2]
	for(c in 1:dC){
		Z<-solve(alpha*E[,,c]-diag(dI))
  		if(c==1){a<-Z}
  		else{ a<-adiag(a,Z)}
  		Y<-0
	}
  	for(c in 1:dC){
    	if(c==1){b<-k[,c]}
    	else{ b<-c(b,k[,c])}
  	}  

	sol<-lsei (A = (1/(1-alpha))*a, B = b, E = array(1,c(1,dI*dC)), F = c(beta), G=diag(dI*dC), H=array(0,c(dI*dC,1)) )
  	return(sol)
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

#####################
# tests
# X <- array(0,c(2,2,2))
# X[,,1] <- matrix(c(0,0.9,0.5,0),nrow=2)
# X[,,2] <- matrix(c(0,0.3,0.7,0),nrow=2)
# beta <- 0.3
# alpha <- 0.1
# N <- Control(X,u,alpha,beta )

#####################
# faire varier les paramètres alpha & beta : 0.01, 0.1 0.5 0.8 0.99
# distribution matrice stochastique
# k distribution 
