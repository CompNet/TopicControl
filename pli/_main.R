########################################################
# Retrieves the data, fits the power laws
# and alternative distributions, compares
# them. All of that using Clauset et al.'s
# source code from from 
# http://tuvalu.santafe.edu/~aaronc/powerlaws/
# 
# setwd("~/eclipse/workspaces/Networks")
# setwd("c:/eclipse/workspaces/Networks")
#
# source("SystProp/pli/_main.R")
########################################################
library("poweRlaw")	# implements partially the functions of Clauset et al.

source("SystProp/pli/discexp.R")
source("SystProp/pli/disclnorm.R")
source("SystProp/pli/discpowerexp.R")
source("SystProp/pli/discweib.R")
source("SystProp/pli/exp.R")							# continuous distribution >> not needed
source("SystProp/pli/lnorm.R")							# continuous distribution >> not needed
source("SystProp/pli/pareto.R")							# continuous distribution >> not needed
source("SystProp/pli/poisson.R")
source("SystProp/pli/powerexp.R")						# continuous distribution >> not needed
source("SystProp/pli/powerexp-exponential-integral.R")	# continuous distribution >> not needed
source("SystProp/pli/power-law-test.R")
source("SystProp/pli/weibull.R")						# continuous distribution >> not needed
source("SystProp/pli/yule.R")
source("SystProp/pli/zeta.R")


########################################################
# retrieving data
########################################################
folder <- "SystProp/pli/temp/"
file.name <- "degree-all"
file.ext <- ".txt"
in.file <- paste(folder,file.name,file.ext,sep="") 
#data <- read.table(in.file) TODO
			data("moby")
			data <- moby

########################################################
# estimating the power law and lower cut-off value
########################################################
# estimate the lower cut-off value
	m <- displ$new(data)
	x.min <- estimate_xmin(m)$xmin
	m$setXmin(x.min)
# estimate the power law exponent
	alpha = estimate_pars(m)$pars
	m$setPars(alpha)
# evaluate the estimated law
	nb.cores <- parallel::detectCores()
	sig <- bootstrap_p(m, no_of_sims=10, threads=nb.cores) #TODO
# init result matrix
	r.names <- c("n","<x>","sd","x_max","^x_min^","^alpha^","n_tail","p")
	pl.results <- matrix(NA, nrow=length(r.names), ncol=1)
	rownames(pl.results) <- r.names
	pl.results["n",1] <- length(data)
	pl.results["<x>",1] <- mean(data)
	pl.results["sd",1] <- sd(data)
	pl.results["x_max",1] <- max(data)
	pl.results["^x_min^",1] <- x.min
	pl.results["^alpha^",1] <- alpha
	pl.results["n_tail",1] <- length(which(data>=x.min))
	pl.results["p",1] <- sig$p
# record those results
	out.file <- paste(folder,file.name,".powerlaw",file.ext,sep="") 
	write.table(x=pl.results, file=out.file, row.names=TRUE, col.names=FALSE)
	

########################################################
# fitting distributions
########################################################
# pure discrete Power law a.k.a. Zipf or Zeta (zeta)
	cat("Fitting discrete power law\n")
	power.d <- zeta.fit(x=data, threshold=x.min, method="ml.direct") # ml.approx
	# out: type, exponent, method, loglike, threshold, samples.over.threshold
# discrete Power law with exponential cutoff (discpowerexp)
	cat("Fitting discrete power law with exponential cut-off\n")
	powerexp.d <- discpowerexp.fit(x=data,threshold=x.min)
	# out: type, exponent, rate, loglike, threshold, samples.over.threshold
# discrete Log-normal distribution (disclnorm)
	cat("Fitting log-normal distribution\n")
	lnorm.d <- fit.lnorm.disc(x=data, threshold=x.min)
	# out: type, meanlog, sdlog, loglike, threshold, datapoints.over.threshold
# discrete Exponential distribution (discexp)
	cat("Fitting exponential distribution\n")
	exp.d <- discexp.fit(x=data, threshold=x.min)
	# out: type, lambda, method, loglike, samples.over.threshold, threshold
# discrete Stretched exponential or Weibull distribution (discweib)
	cat("Fitting stretched exponential distribution\n")
	weib.d <- discweib.fit(x=data, threshold=x.min)
	# out: type, shape, scale, loglike, threshold, samples.over.threshold
# Poisson distribution
	cat("Fitting poisson distribution\n")
	pois.d <- pois.tail.fit(x=data, threshold=x.min)
	# out: type, rate, loglike, threshold, samples.over.threshold, full.mean, mean.over.threshold
# Yule-Simon distribution (yule)
	cat("Fitting yule-simon distribution\n")
	yule.d <- yule.fit(x=data, threshold=x.min)
	# out: type, exponent, loglike, threshold, samples.over.threshold


########################################################
# comparing distributions
########################################################
# init result matrix
	r.names <- c("PowerExp","LogNorm","Exp","StrtExp","Poisson","YuleSimon")
	c.names <- c("LLRatio","p1Val","p2Val")
	comp.results <- matrix(NA,ncol=length(c.names),nrow=length(r.names))
	rownames(comp.results) <- r.names
	colnames(comp.results) <- c.names
# pure power law vs. power law with exponential cutoff
	cat("Comparing power law vs. power law with exponential cutoff\n")
	powerexp.res <- power.powerexp.lrt(power.d=power.d, powerexp.d=powerexp.d)
	comp.results["PowerExp","LLRatio"] <- powerexp.res$log.like.ratio
	comp.results["PowerExp","p1Val"] <- powerexp.res$p_value
	comp.results["PowerExp","p2Val"] <- NA
# power law vs. log-normal
	cat("Comparing power law vs. log-normal distribution\n")
	lnorm.res <- vuong(zeta.lnorm.llr(x=data, zeta.d=power.d, lnorm.d=lnorm.d))
	comp.results["LogNorm","LLRatio"] <- lnorm.res$loglike.ratio
	comp.results["LogNorm","p1Val"] <- lnorm.res$p.one.sided
	comp.results["LogNorm","p2Val"] <- lnorm.res$p.two.sided
# power law vs. exponential
	cat("Comparing power law vs. exponential distribution\n")
	exp.res <- vuong(zeta.exp.llr(x=data, zeta.d=power.d, exp.d=exp.d))
	comp.results["Exp","LLRatio"] <- exp.res$loglike.ratio
	comp.results["Exp","p1Val"] <- exp.res$p.one.sided
	comp.results["Exp","p2Val"] <- exp.res$p.two.sided
# power law vs. stretched exponential
	cat("Comparing power law vs. stretched exponential distribution\n")
	weib.res <- vuong(zeta.weib.llr(x=data, zeta.d=power.d, weib.d=weib.d))
	comp.results["StrtExp","LLRatio"] <- weib.res$loglike.ratio
	comp.results["StrtExp","p1Val"] <- weib.res$p.one.sided
	comp.results["StrtExp","p2Val"] <- weib.res$p.two.sided
# power law vs. poisson
	cat("Comparing power law vs. poisson distribution\n")
	pois.res <- vuong(zeta.poisson.llr(x=data, zeta.d=power.d, pois.d=pois.d))
	comp.results["Poisson","LLRatio"] <- pois.res$loglike.ratio
	comp.results["Poisson","p1Val"] <- pois.res$p.one.sided
	comp.results["Poisson","p2Val"] <- pois.res$p.two.sided
# power law vs. yule
	cat("Comparing power law vs. yule-simon distribution\n")
	yule.res <- vuong(zeta.yule.llr(x=data, zeta.d=power.d, yule.d=yule.d))
	comp.results["YuleSimon","LLRatio"] <- yule.res$loglike.ratio
	comp.results["YuleSimon","p1Val"] <- yule.res$p.one.sided
	comp.results["YuleSimon","p2Val"] <- yule.res$p.two.sided
# record comparison results
	cat("Recording results\n")
	out.file <- paste(folder,file.name,".comparisons",file.ext,sep="") 
	write.table(x=comp.results, file=out.file, row.names=TRUE, col.names=TRUE)
	
	
cat("All done\n")
########################################################
########################################################
