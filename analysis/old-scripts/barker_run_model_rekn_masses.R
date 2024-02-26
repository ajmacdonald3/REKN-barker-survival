################################################################################
# Run Barker model on real REKN data with covariates
#
################################################################################

library(tidyverse)
library(lubridate)
#library(R2ucare)
library(R2jags)
library(jagsUI)
library(corrplot)

################################################################################

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
for (i in 1:nind){
   for (t in first[i]:(n.occasions-1)){
   logit(surv[i,t]) <- mu.s + iv*beta*cov[t] + epsilon.s[t]
   
   logit(Fid[i,t]) <- mu.F + epsilon.F[t]
   
   logit(psight[i,t]) <- mu.p + epsilon.p[t]
   
   logit(Rst[i,t]) <- mu.R + epsilon.R[t]
   
   Rp[i,t] <- mean.Rp
   
   f[i,t] <- mean.f
   } # t
} # i

for (t in 1:(n.occasions-1)){
   epsilon.s[t] ~ dnorm(0, tau.s)T(-15, 15)
   s[t] <- 1 / (1+exp(-mu.s-iv*beta*cov[t]-epsilon.s[t]))
     
   epsilon.F[t] ~ dnorm(0, tau.F)T(-15, 15)
   F[t] <- 1 / (1+exp(-mu.F-epsilon.F[t]))
   
   epsilon.p[t] ~ dnorm(0, tau.p)T(-15, 15)
   p[t] <- 1 / (1+exp(-mu.p-epsilon.p[t]))

   epsilon.R[t] ~ dnorm(0, tau.R)T(-15, 15)
   R[t] <- 1 / (1+exp(-mu.R-epsilon.R[t]))

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

beta ~ dnorm(0, 0.001)T(-10, 10) # Prior for slope parameter
iv ~ dbern(0.5) # Prior for indicator variable

# Define state-transition and observation matrices 	
for (i in 1:nind){
   # Define probabilities of state S(t+1) given S(t)
   for (t in first[i]:(n.occasions-1)){
      ps[1,i,t,1] <- surv[i,t]*Fid[i,t]
      ps[1,i,t,2] <- surv[i,t]*(1-Fid[i,t])
      ps[1,i,t,3] <- (1-surv[i,t])*f[i,t]
      ps[1,i,t,4] <- (1-surv[i,t])*Rp[i,t]*(1-f[i,t])
      ps[1,i,t,5] <- (1-surv[i,t])*(1-Rp[i,t])*(1-f[i,t])
      ps[2,i,t,1] <- 0
      ps[2,i,t,2] <- surv[i,t]
      ps[2,i,t,3] <- (1-surv[i,t])*f[i,t]
      ps[2,i,t,4] <- (1-surv[i,t])*Rp[i,t]*(1-f[i,t])
      ps[2,i,t,5] <- (1-surv[i,t])*(1-Rp[i,t])*(1-f[i,t])
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
      po[1,i,t,1] <- psight[i,t]*Rst[i,t]
      po[1,i,t,2] <- psight[i,t]*(1-Rst[i,t])
      po[1,i,t,3] <- (1-psight[i,t])*Rst[i,t]
      po[1,i,t,4] <- (1-psight[i,t])*(1-Rst[i,t])
      po[1,i,t,5] <- 0
      po[2,i,t,1] <- 0
      po[2,i,t,2] <- 0
      po[2,i,t,3] <- Rst[i,t]
      po[2,i,t,4] <- 1-Rst[i,t]
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
   #z[i,first[i]] <- y[i,first[i]]
   z[i,first[i]] <- 1
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

################################################################################

# load covariates
rekn_masses <- read_csv("./data/rekn-masses/rekn_masses.csv")

# standardize covariates
rekn_covs_std <- rekn_masses %>% 
  mutate(arr_mass_std = (arr_mass - mean(arr_mass))/sd(arr_mass),
         dep_mass_std = (dep_mass - mean(dep_mass))/sd(dep_mass),
         mass_gain_std = (mass_gain - mean(mass_gain))/sd(mass_gain),
         p180_std = (p180 - mean(p180))/sd(p180)) %>% 
  dplyr::select(-arr_mass, -dep_mass, -mass_gain, -p180, -n_arr, -n_dep, -hsc)

saveRDS(rekn_covs_std, file = "./processed-data/rekn_covariates_std.rds")

# set up covariates
rekn_covs <- rekn_covs_std %>% 
  filter(year %in% 2010:2019) %>% 
  select(-year)

covs_mat <- sapply(rekn_covs, as.numeric)
covariates <- c("arr-mass", "dep-mass", "mass-gain", "p180")

# correlation matrix
covs_cor <- cor(rekn_covs)
corrplot(covs_cor)

# load encounter history data
barker_enchist <- readRDS("processed-data/barker-enchist.rds")

# convert BirdID to row names so a numeric matrix can be generated
rownames(barker_enchist) <- NULL

barker_enchist <- barker_enchist %>% 
  column_to_rownames(var = "BirdID")

barker_enchist <- sapply(barker_enchist, as.numeric)

# compute date of first capture
get.first <- function(x) min(which(x!=0))
first <- apply(barker_enchist, 1, get.first)

# recode CH matrix: note, a 0 is not allowed!
# 1 = seen in, seen out; 2 = seen in, not seen out; 3 = not seen in, seen out; 4 = not seen in, not seen out; 5 = recovered
r_barker_enchist <- barker_enchist  # recoded CH
r_barker_enchist[r_barker_enchist==0] <- 4

# initial values
z.init <- matrix(1, nrow(r_barker_enchist), ncol(r_barker_enchist))
for (i in 1:nrow(r_barker_enchist)){
  z.init[i,(1:first[i])] <- NA
}

inits <- function(){list(mean.s = runif(1, 0, 1), mean.F = runif(1, 0, 1), mean.p = runif(1, 0, 1),
                         mean.R = runif(1, 0, 1), mean.Rp = runif(1, 0, 1),
                         sigma.s = runif(1, 0, 5), sigma.F = runif(1, 0, 5), sigma.p = runif(1, 0, 5),
                         sigma.R = runif(1, 0, 5), 
                         iv = rep(1, 1), beta = runif(1, -5, 5), z = z.init)}

# parameters monitored
parameters <- c("mean.s", "mean.F", "mean.p", "mean.R", "mean.Rp", "mean.f",
                "sigma2.s", "sigma2.F", "sigma2.p", "sigma2.R",
                "s", "F", "p", "R", "beta", "iv")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 25000
nc <- 3

# run model for each climate variable
system.time({
  for (z in 1:ncol(covs_mat)){
    
    # bundle data
    jags.data <- list(y = r_barker_enchist, first = first, n.occasions = dim(r_barker_enchist)[2],
                      nind = dim(r_barker_enchist)[1], cov = covs_mat[,z])
    
    # run model
    barker.mod <- jagsUI::jags(jags.data, inits, parameters, "barker.jags", n.chains = nc, n.thin = nt,
                               n.iter = ni, n.burnin = nb, parallel = TRUE)
    
    # save results
    save(barker.mod,
         file = paste0("./analysis-output/covariate-models/", covariates[z], "/model_", covariates[z], ".RData"))
    
  }
})

################################################################################
# plot results
################################################################################

#### arr-mass ####
load("./analysis-output/covariate-models/arr-mass/model_arr-mass.RData")

print(barker.mod)

#### dep-mass ####
load("./analysis-output/covariate-models/dep-mass/model_dep-mass.RData")

print(barker.mod)

#### mass-gain ####
load("./analysis-output/covariate-models/mass-gain/model_mass-gain.RData")

print(barker.mod)

#### p180 ####
load("./analysis-output/covariate-models/p180/model_p180.RData")

print(barker.mod)
