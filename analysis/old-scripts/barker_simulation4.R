# Barker model simulation

library(jagsUI)

# Specify model in BUGS language
sink("barker.jags")
cat("
model {

# -------------------------------------------------
# Parameters:
# s: survival probability
# F: fidelity probability
# p: inside resighting probability
# R: outside resighting probability given survival from t to t+1
# Rp: outside resighting probability given mortality during interval t to t+1
# f: recovery probability
# -------------------------------------------------
# States (S):
# 1 alive in study area
# 2 alive outside study area
# 3 recently dead and recovered
# 4 recently dead, resighted
# 5 dead in previous interval
# Observations (O):
# 1 seen in, seen out
# 2 seen in, not seen out
# 3 not seen in, seen out
# 4 not seen in, not seen out
# 5 recovered
# -------------------------------------------------

# Priors and constraints
for (t in 1:(n.occasions-1)){
   
   logit(s[t]) <- mu.s + epsilon.s[t]
   epsilon.s[t] ~ dnorm(0, tau.s)T(-15, 15)
   
   logit(F[t]) <- mu.F + epsilon.F[t]
   epsilon.F[t] ~ dnorm(0, tau.F)T(-15, 15)
   
   logit(p[t]) <- mu.p + epsilon.p[t]
   epsilon.p[t] ~ dnorm(0, tau.p)T(-15, 15)
   
   logit(R[t]) <- mu.R + epsilon.R[t]
   epsilon.R[t] ~ dnorm(0, tau.R)T(-15, 15)
   
   Rp[t] <- mean.Rp
   
   f[t] <- mean.f
}

mu.s <- log(mean.s / (1-mean.s)) # logit transformation   
mean.s ~ dunif(0, 1)             # prior for mean survival
tau.s <- pow(sigma.s, -2)
sigma.s ~ dunif(0, 5)            # prior on standard deviation
sigma2.s <- pow(sigma.s, 2)      # temporal variance

mu.F <- log(mean.F / (1-mean.F)) # logit transformation   
mean.F ~ dunif(0, 1)             # prior for mean fidelity
tau.F <- pow(sigma.F, -2)
sigma.F ~ dunif(0, 5)            # prior on standard deviation
sigma2.F <- pow(sigma.F, 2)      # temporal variance

mu.p <- log(mean.p / (1-mean.p)) # logit transformation   
mean.p ~ dunif(0, 1)             # prior for mean survival
tau.p <- pow(sigma.p, -2)
sigma.p ~ dunif(0, 5)            # prior on standard deviation
sigma2.p <- pow(sigma.p, 2)      # temporal variance

mu.R <- log(mean.R / (1-mean.R)) # logit transformation   
mean.R ~ dunif(0, 1)             # prior for mean survival
tau.R <- pow(sigma.R, -2)
sigma.R ~ dunif(0, 5)            # prior on standard deviation
sigma2.R <- pow(sigma.R, 2)      # temporal variance

mean.Rp ~ dbeta(1, 1)            # prior for mean resighting outside given mortality
mean.f <- 0                      # prior for mean recovery

# Define state-transition and observation matrices 	
for (i in 1:nind){
   # Define probabilities of state S(t+1) given S(t)
   for (t in first[i]:(n.occasions-1)){
      ps[1,i,t,1] <- s[t]*F[t]
      ps[1,i,t,2] <- s[t]*(1-F[t])
      ps[1,i,t,3] <- (1-s[t])*f[t]
      ps[1,i,t,4] <- (1-s[t])*Rp[t]*(1-f[t])
      ps[1,i,t,5] <- (1-s[t])*(1-Rp[t])*(1-f[t])
      ps[2,i,t,1] <- 0
      ps[2,i,t,2] <- s[t]
      ps[2,i,t,3] <- (1-s[t])*f[t]
      ps[2,i,t,4] <- (1-s[t])*Rp[t]*(1-f[t])
      ps[2,i,t,5] <- (1-s[t])*(1-Rp[t])*(1-f[t])
      ps[3,i,t,1] <- 0
      ps[3,i,t,2] <- 0
      ps[3,i,t,3] <- 0
      ps[3,i,t,4] <- 0
      ps[3,i,t,5] <- 1
      ps[4,i,t,1] <- 0
      ps[4,i,t,2] <- 0
      ps[4,i,t,3] <- 0
      ps[4,i,t,4] <- 0
      ps[4,i,t,5] <- 1
      ps[5,i,t,1] <- 0
      ps[5,i,t,2] <- 0
      ps[5,i,t,3] <- 0
      ps[5,i,t,4] <- 0
      ps[5,i,t,5] <- 1

      # Define probabilities of O(t) given S(t)
      po[1,i,t,1] <- p[t]*R[t]
      po[1,i,t,2] <- p[t]*(1-R[t])
      po[1,i,t,3] <- (1-p[t])*R[t]
      po[1,i,t,4] <- (1-p[t])*(1-R[t])
      po[1,i,t,5] <- 0
      po[2,i,t,1] <- 0
      po[2,i,t,2] <- 0
      po[2,i,t,3] <- R[t]
      po[2,i,t,4] <- 1-R[t]
      po[2,i,t,5] <- 0
      po[3,i,t,1] <- 0
      po[3,i,t,2] <- 0
      po[3,i,t,3] <- 0
      po[3,i,t,4] <- 0
      po[3,i,t,5] <- 1
      po[4,i,t,1] <- 0
      po[4,i,t,2] <- 0
      po[4,i,t,3] <- 1
      po[4,i,t,4] <- 0
      po[4,i,t,5] <- 0
      po[5,i,t,1] <- 0
      po[5,i,t,2] <- 0
      po[5,i,t,3] <- 0
      po[5,i,t,4] <- 1
      po[5,i,t,5] <- 0
      } #t
   } #i

# Likelihood 
for (i in 1:nind){
   # Define latent state at first capture
   z[i,first[i]] <- y[i,first[i]]
   for (t in (first[i]+1):n.occasions){
      # State process: draw S(t) given S(t-1)
      z[i,t] ~ dcat(ps[z[i,t-1], i, t-1,])
      # Observation process: draw O(t) given S(t)
      y[i,t] ~ dcat(po[z[i,t], i, t-1,])
      } #t
   } #i
}
",fill = TRUE)
sink()

# 9.5. Joint analysis of capture-recapture and mark-recovery data
# 9.5.1. Model description
# 9.5.2. Generation of simulated data

# Define function to simulate multistate capture-recapture data
simul.ms <- function(PSI.STATE, PSI.OBS, marked, unobservable = NA){
  # Unobservable: number of state that is unobservable
  n.occasions <- dim(PSI.STATE)[4] + 1
  CH <- CH.TRUE <- matrix(NA, ncol = n.occasions, nrow = sum(marked))
  # Define a vector with the occasion of marking
  mark.occ <- matrix(0, ncol = dim(PSI.STATE)[1], nrow = sum(marked))
  g <- colSums(marked)
  for (s in 1:dim(PSI.STATE)[1]){
    if (g[s]==0) next  # To avoid error message if nothing to replace
    mark.occ[(cumsum(g[1:s])-g[s]+1)[s]:cumsum(g[1:s])[s],s] <-
      rep(1:n.occasions, marked[1:n.occasions,s])
  } #s
  for (i in 1:sum(marked)){
    for (s in 1:dim(PSI.STATE)[1]){
      if (mark.occ[i,s]==0) next
      first <- mark.occ[i,s]
      CH[i,first] <- s
      CH.TRUE[i,first] <- s
    } #s
    for (t in (first+1):n.occasions){
      # Multinomial trials for state transitions
      if (first==n.occasions) next
      state <- which(rmultinom(1, 1, PSI.STATE[CH.TRUE[i,t-1],,i,t-1])==1)
      CH.TRUE[i,t] <- state
      # Multinomial trials for observation process
      event <- which(rmultinom(1, 1, PSI.OBS[CH.TRUE[i,t],,i,t-1])==1)
      CH[i,t] <- event
    } #t
  } #i
  # Replace the NA and the highest state number (dead) in the file by 0
  CH[is.na(CH)] <- 0
  CH[CH==dim(PSI.STATE)[1]] <- 0
  CH[CH==unobservable] <- 0
  id <- numeric(0)
  for (i in 1:dim(CH)[1]){
    z <- min(which(CH[i,]!=0))
    ifelse(z==dim(CH)[2], id <- c(id,i), id <- c(id))
  }
  return(list(CH=CH[-id,], CH.TRUE=CH.TRUE[-id,]))
  # CH: capture histories to be used
  # CH.TRUE: capture histories with perfect observation
}

n.sims <- 100
n.occasions <- 10  
n.states <- 4
n.obs <- 5
marked <- matrix(0, ncol = n.states, nrow = n.occasions)
marked[,1] <- rep(100, n.occasions)	# Releases in study area

# MCMC settings
ni <- 10000
nt <- 5
nb <- 5000
nc <- 3

# arrays to store results
sim.params <- matrix(NA, 6, n.sims)
results.barker <- array(NA, dim = c(6, n.sims, 3))

for (i in 1:n.sims){
  print(i)
  print(Sys.time())
  
  # simulate parameter values
  phi <- runif(1, 0.7, 0.9)
  F <- runif(1, 0.8, 0.95)
  gammapp <- runif(1, 0.6, 0.95)
  gammap <- gammapp
  p <- runif(n.sec, 0.15, 0.35)
  pstar <- 1 - prod(1 - p)
  r <- runif(1, 0.15, 0.35)
  R <- runif(1, 0.15, 0.35)
  Rp <- R/2
  
  
  
  # store simulated parameter values
  sim.params[1,ii] <- phi
  sim.params[2,ii] <- F
  sim.params[3,ii] <- gammapp
  sim.params[4,ii] <- gammap
  sim.params[5,ii] <- pstar
  sim.params[6,ii] <- r
  sim.params[7,ii] <- R
  sim.params[8,ii] <- Rp
  
