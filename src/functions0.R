# Cette fonction calcule l'équilibre de notre système. Les entrées sont 
# la matrice d'adjacence E (dimension I^2), la matrice des flows externes 
# (dimension: IxC), et enfin le vecteur d'activation (dimension: I).
equilibre<- function(E,lambda,alpha,npar=TRUE,print=TRUE) {
  Lambda<-sum(lambda)+sum(alpha) # creation de sum_{j,c}\lambda_{j,c}+\alpha_j
  lambda<-lambda*(1/Lambda) # creation de \frac{\lambda_{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  alpha<-alpha*(1/Lambda)  # creation de \frac{\alpha{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  E<-E*alpha # nouvelle matrice d'adjacence
  dI<-dim(E)[1]
  dC<-dim(lambda)[2]
  x<-array(0,c(dI,dC))
  # calcul de l'équilibre
  for(c in 1:dC){
    x[,c]<-solve(E-diag(dI),-lambda[,c])
  }
  return(x)
} 

# Cette function calcule lambda, sachant un k donné. 
# k est une matrice de dimension IxC.
control<- function(E, k, alpha, npar=TRUE, print=TRUE){
  dI <- dim(k)[1]
  dC <- dim(k)[2]
  lambda <- array(0,c(dI,dC))
  G <- (E%*%k)*alpha
  H <- E%*%k
  Lambda<-(sum(k)*sum(alpha)-sum(G))/(1-sum(k))
  for(i in 1:dI){
    for(c in 1:dC){
      lambda[i,c] <- k[i,c]*Lambda+k[i,c]*sum(alpha)-alpha[i]*(H[i,c])
    }
  }
  return(lambda)
}

simulation<- function(E,lambda,alpha,t,npar=TRUE,print=TRUE) {
  Lambda <- sum(lambda)+sum(alpha) # creation de sum_{j,c}\lambda_{j,c}+\alpha_j
  lambda <- lambda*(1/Lambda) # creation de \frac{\lambda_{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  alpha <- alpha*(1/Lambda)  # creation de \frac{\alpha{i,c'}}{sum_{j,c}\lambda_{j,c}+\alpha_j}
  E <- E*alpha # nouvelle matrice d'adjacence
  dI <- dim(E)[1]
  dC <- dim(lambda)[2]
  x <- array(0,c(dI,dC,t))
  # calcul de l'équilibre
  for(t in 1:t){
  	for(c in 1:dC){
		x[,c,t] <- as.vector((E-diag(dI)) %*% x[,c,t] + lambda[,c])
	}   
  }
 
  return(x)
}
