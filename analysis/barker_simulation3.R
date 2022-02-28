library(jagsUI)

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

n.occasions <- 10  
n.states <- 4
n.obs <- 5
marked <- matrix(0, ncol = n.states, nrow = n.occasions)
marked[,1] <- rep(100, n.occasions)	# Releases in study area

# Define mean survival, transitions, recapture, as well as number of occasions, states, observations and released individuals 

mean.s <- 0.7 # mean survival
var.s <- 0.5 # temporal variance of survival
logit.s <- rnorm(n.occasions-1, qlogis(mean.s), var.s^0.5)
s <- plogis(logit.s) # survival

mean.F <- 0.8
var.F <- 0.5
logit.F <- rnorm(n.occasions-1, qlogis(mean.F), var.F^0.5)
F <- plogis(logit.F) # site fidelity

mean.p <- 0.5
var.p <- 0.5
logit.p <- rnorm(n.occasions-1, qlogis(mean.p), var.p^0.5)
p <- plogis(logit.p) # james bay resighting probability

mean.R <- 0.25
var.R <- 0.5
logit.R <- rnorm(n.occasions-1, qlogis(mean.R), var.R^0.5)
R <- plogis(logit.R) # non-james bay resighting probability given survival from t to t+1

Rp <- mean.R/2 # non-james bay resighting probability given mortality from t to t+1

f <- 0 # band recovery probability

# Define matrices with survival, transition and recapture probabilities
# These are 4-dimensional matrices, with 
# Dimension 1: state of departure
# Dimension 2: state of arrival
# Dimension 3: individual
# Dimension 4: time
# 1. State process matrix
totrel <- sum(marked)*(n.occasions-1)
PSI.STATE <- array(NA, dim=c(n.states, n.states, totrel, n.occasions-1))
for (i in 1:totrel){
  for (t in 1:(n.occasions-1)){
    PSI.STATE[,,i,t] <- matrix(c(
      s[t]*F[t], s[t]*(1-F[t]), (1-s[t]), 0,
      0,   s[t],       (1-s[t]), 0,
      0,   0,       0,     1,
      0,   0,       0,     1),
      nrow = n.states, byrow = TRUE)
  } #t
} #i

# 2.Observation process matrix
PSI.OBS <- array(NA, dim=c(n.states, n.obs, totrel, n.occasions-1))
for (i in 1:totrel){
  for (t in 1:(n.occasions-1)){
    PSI.OBS[,,i,t] <- matrix(c(
      p[t]*R[t], p[t]*(1-R[t]), (1-p[t])*R[t],  (1-p[t])*(1-R[t]),  0,
      0,   0,       R[t],        (1-R[t]),        0,
      0,   0,       Rp*(1-f), (1-Rp)*(1-f), f,
      0,   0,       0,         1,           0),
      nrow = n.states, byrow = TRUE)
  } #t
} #i

# Execute simulation function
sim <- simul.ms(PSI.STATE, PSI.OBS, marked)
CH <- sim$CH

# Compute date of first capture
get.first <- function(x) min(which(x!=0))
first <- apply(CH, 1, get.first)

# Recode CH matrix: note, a 0 is not allowed!
# 1 = seen in, seen out; 2 = seen in, not seen out; 3 = not seen in, seen out; 4 = not seen in, not seen out; 5 = recovered
rCH <- CH  # Recoded CH
rCH[rCH==0] <- 4

# 9.5.3. Analysis of the model
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

# Initial values
# In order to run the model, we must provide sound initial values for the true latent state z. The difficulty is that 
# observed states do not always correspond to the true latent state. For example, in our observation the state 2 refers 
# to an individual whose ring has been reported, while the true state 2 refers to an individuals that is alive, but 
# outside the study area. Consequently, we cannot use the observed states as the initial values for the true state in JAGS 
# (in BUGS this works well). The function known.ld provides initial values for the state z for our model. The important 
# things are i) that the observations correspond to the true state (i.e. all "2" are converted into "3"), ii) that states 
# after a "3" are all "4", iii) that all non-observations between "1" become "1", and iv) that all remaining originally "3" 
# after the first observation become "1".

# ld.init <- function(ch, first){
#   ch[ch==4] <- NA
#   v0 <- which(ch==2, arr.ind = T)
#   ch[v0] <- 1
#   v1 <- which(ch==3, arr.ind = T)
#   ch[v1] <- 2
#   #v2 <- which(ch==5, arr.ind = T)
#   #ch[v2] <- 3
#   #for (i in 1:nrow(v2)){
#   # ifelse(v2[i,2]!=ncol(ch), ch[v2[i,1], (v2[i,2]+1):ncol(ch)] <- 5, next)}
#   for (i in 1:nrow(ch)){
#     m1 <- max(which(ch[i,]==1))
#     ch[i,first[i]:m1] <- 1
#   }
#   #for (i in 1:nrow(v2)){
#   #  u1 <- min(which(ch[v2[i,1],]==1))
#   #  ch[v2[i],u1:(v2[i,2]-1)] <- 1
#   #}
#   # transpose and fill here for 1s and 2s
#   ch <- as.data.frame(t(ch))
#   ch <- tidyr::fill(ch, names(ch))
#   ch <- t(ch)
#   
#   for (i in 1:nrow(ch)){
#     for (j in first[i]:ncol(ch)){
#       if(is.na(ch[i,j])==1) ch[i,j] <- 1
#     }
#     ch[i,first[i]] <- NA
#   }
#   return(ch)
# }

z.init <- matrix(1, nrow(rCH), ncol(rCH))
for (i in 1:nrow(rCH)){
  z.init[i,(1:first[i])] <- NA
}

inits <- function(){list(mean.s = runif(1, 0, 1), mean.F = runif(1, 0, 1), mean.p = runif(1, 0, 1),
                         mean.R = runif(1, 0, 1), mean.Rp = runif(1, 0, 1),
                         sigma.s = runif(1, 0, 5), sigma.F = runif(1, 0, 5), sigma.p = runif(1, 0, 5),
                         sigma.R = runif(1, 0, 5), z = z.init)}

# Bundle data
# ch <- rCH
# ch[ch==4] <- NA
# z.known <- ld.init(rCH, first)
# z.known[is.na(ch)] <- NA
# for (i in 1:nrow(ch)){
#   z.known[i,f[i]] <- NA
# }
# z <- ld.init(rCH, first)
# z[!is.na(ch)] <- NA

jags.data <- list(y = rCH, first = first, n.occasions = dim(rCH)[2], nind = dim(rCH)[1])

# Parameters monitored
parameters <- c("mean.s", "mean.F", "mean.p", "mean.R", "mean.Rp", "mean.f",
                "sigma2.s", "sigma2.F", "sigma2.p", "sigma2.R",
                "s", "F", "p", "R")

# MCMC settings
ni <- 10000
nt <- 5
nb <- 5000
nc <- 3

barker.mod <- jags(jags.data, inits, parameters, "barker.jags", n.chains = nc, n.thin = nt,
                   n.iter = ni, n.burnin = nb, parallel = TRUE)

print(barker.mod, digits = 3)
