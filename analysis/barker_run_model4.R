################################################################################
# Run Barker model on real REKN data with principal component covariates
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
   logit(surv[i,t]) <- mu.s + iv[1]*beta[1]*pc1[t] + iv[2]*beta[2]*pc2[t] + epsilon.s[t]
   
   logit(Fid[i,t]) <- mu.F + epsilon.F[t]
   
   logit(psight[i,t]) <- mu.p + epsilon.p[t]
   
   logit(Rst[i,t]) <- mu.R + epsilon.R[t]
   
   Rp[i,t] <- mean.Rp
   
   f[i,t] <- mean.f
   } # t
} # i

for (t in 1:(n.occasions-1)){
   epsilon.s[t] ~ dnorm(0, tau.s)T(-15, 15)
   s[t] <- 1 / (1+exp(-mu.s - iv[1]*beta[1]*pc1[t] - iv[2]*beta[2]*pc2[t] - epsilon.s[t]))
     
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

for (j in 1:2){
  beta[j] ~ dnorm(0, 0.001)T(-5, 5) # Prior for slope parameter
  iv[j] ~ dbern(0.5) # Prior for indicator variable
}

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

# load standardized covariates
surv_covs <- readRDS("./processed-data/surv_pc_scores.rds")
covs_mat <- surv_covs[,1:2]
covariates <- c("pc-1", "pc-2")

# correlation matrix
covs_cor <- cor(covs_mat)
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
                         iv = rep(1, 2), beta = runif(2, -5, 5), z = z.init)}

# parameters monitored
parameters <- c("mean.s", "mean.F", "mean.p", "mean.R", "mean.Rp", "mean.f",
                "sigma2.s", "sigma2.F", "sigma2.p", "sigma2.R",
                "s", "F", "p", "R", "beta", "iv")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 25000
nc <- 3

# run model
system.time({
    
    # bundle data
    jags.data <- list(y = r_barker_enchist, first = first, n.occasions = dim(r_barker_enchist)[2],
                      nind = dim(r_barker_enchist)[1], pc1 = covs_mat[,1], pc2 = covs_mat[,2])
    
    # run model
    barker.mod <- jagsUI::jags(jags.data, inits, parameters, "barker.jags", n.chains = nc, n.thin = nt,
                               n.iter = ni, n.burnin = nb, parallel = TRUE)
    
    # save results
    save(barker.mod,
         file = "./analysis-output/covariate-models/pca/model_1.RData")
    
})

################################################################################
# plot results
################################################################################

load("./analysis-output/covariate-models/pca/model_1.RData")

print(barker.mod)

# traceplots
pdf(file = paste0("./analysis-output/covariate-models/pca/traceplots/barker-pca-traceplots.pdf"))
par(mfrow = c(3, 3))
traceplot(barker.mod)
dev.off()

# parameter identifiability checks
sims.list <- barker.mod$sims.list
sims.list <- as.data.frame(sims.list)

theme_set(theme_bw())

for (i in colnames(sims.list)){
  
  png(filename = paste0("analysis-output/covariate-models/pca/parameter-identifiability/",
                        i,"-","check.png"),
      width=4, height=3, units="in", res=600)
  
  print(ggplot(sims.list, aes(sims.list[,i])) +
          geom_density() +
          #geom_hline(yintercept = 1, linetype = "dashed") +
          xlab(i))
  
  dev.off()
  
}

# plot estimates

windowsFonts(Times=windowsFont("TT Times New Roman"))

# set custom theme for all plots
theme_cust <- function() {
  theme_classic(base_family = "Times") %+replace%
    theme(axis.title.x = element_text(size=12),
          axis.text.x  = element_text(size=10, colour = "black"),
          axis.title.y = element_text(size=12, angle = 90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_text(size=10, colour = "black"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          strip.text.x = element_text(size=12, face = "bold"),
          legend.text = element_text(size=10),
          legend.key.height = unit(1, "mm"),
          plot.title = element_text(size = 12, hjust = 0, vjust = 1.5),
          #panel.border = element_rect(size =0.5, fill = "transparent"),
          plot.margin = margin(10, 10, 10, 15))
}

# format data
barker_res <- barker.mod$summary

barker_res <- barker_res %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  dplyr::rename(parameter = rowname) %>% 
  dplyr::rename(lcl = `2.5%`) %>% 
  dplyr::rename(median = `50%`) %>% 
  dplyr::rename(ucl = `97.5%`) %>% 
  select(parameter, mean, lcl, median, ucl)

s_res <- barker_res %>% 
  filter(str_detect(parameter, "s\\[")) %>% 
  mutate(year = 2009:2018) %>% 
  mutate(var = "s")

fid_res <- barker_res %>% 
  filter(str_detect(parameter, "F\\[")) %>% 
  mutate(year = 2009:2018) %>% 
  mutate(var = "fid")

p_res <- barker_res %>% 
  filter(str_detect(parameter, "p\\[")) %>% 
  mutate(year = 2010:2019) %>% 
  mutate(var = "p")

osr_res <- barker_res %>% 
  filter(str_detect(parameter, "R\\[")) %>% 
  mutate(year = 2010:2019) %>% 
  mutate(var = "osr")

# apparent annual survival
s_plot <- ggplot() +
  geom_rect(data = filter(barker_res, parameter == "mean.s"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.s"),
             aes(yintercept=mean)) +
  geom_errorbar(data = s_res, aes(x=as.factor(year), ymin=lcl, ymax=ucl),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
            linetype="dashed", size=0.5) +
  geom_point(data = s_res, aes(x=as.factor(year), y=mean),
             size=3) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0.6, 1) +
  ylab("Annual survival probability") +
  theme_cust() +
  theme(axis.title.x = element_blank())

png(filename = "analysis-output/covariate-models/pca/figures/s-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_plot)

dev.off()

# transience plot
fid_plot <- ggplot() +
  geom_rect(data = filter(barker_res, parameter == "mean.F"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.F"),
             aes(yintercept=mean)) +
  geom_errorbar(data = fid_res, aes(x=as.factor(year), ymin=lcl, ymax=ucl),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = fid_res, aes(x=as.factor(year), y=mean, group = var),
            linetype="dashed", size=0.5) +
  geom_point(data = fid_res, aes(x=as.factor(year), y=mean),
             size=3) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0.4, 1) +
  ylab("Site fidelity probability") +
  theme_cust() +
  theme(axis.title.x = element_blank())

png(filename = "analysis-output/covariate-models/pca/figures/fid-plot.png",
    width=6, height=4, units="in", res=600)

plot(fid_plot)

dev.off()

# resighting plot
p_plot <- ggplot() +
  geom_rect(data = filter(barker_res, parameter == "mean.p"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.p"),
             aes(yintercept=mean)) +
  geom_errorbar(data = p_res, aes(x=as.factor(year), ymin=lcl, ymax=ucl),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = p_res, aes(x=as.factor(year), y=mean, group = var),
            linetype="dashed", size=0.5) +
  geom_point(data = p_res, aes(x=as.factor(year), y=mean),
             size=3) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0, 1) +
  ylab("Resighting probability") +
  theme_cust() +
  theme(axis.title.x = element_blank())

png(filename = "analysis-output/covariate-models/pca/figures/p-plot.png",
    width=6, height=4, units="in", res=600)

plot(p_plot)

dev.off()

# off-site resighting probability
osr_plot <- ggplot() +
  geom_rect(data = filter(barker_res, parameter == "mean.R"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.R"),
             aes(yintercept=mean)) +
  geom_errorbar(data = osr_res, aes(x=as.factor(year), ymin=lcl, ymax=ucl),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = osr_res, aes(x=as.factor(year), y=mean, group = var),
            linetype="dashed", size=0.5) +
  geom_point(data = osr_res, aes(x=as.factor(year), y=mean),
             size=3) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0.4, 0.8) +
  ylab("Off-site resighting probability") +
  theme_cust() +
  theme(axis.title.x = element_blank())

png(filename = "analysis-output/covariate-models/pca/figures/osr-plot.png",
    width=6, height=4, units="in", res=600)

plot(osr_plot)

dev.off()

# both resighting probabilities
psight_res <- bind_rows(p_res, osr_res)

psight_plot <- ggplot() +
  geom_rect(data = filter(barker_res, parameter == "mean.R"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.R"),
             aes(yintercept=mean)) +
  geom_rect(data = filter(barker_res, parameter == "mean.p"),
            aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(barker_res, parameter == "mean.p"),
             aes(yintercept=mean)) +
  geom_errorbar(data = psight_res, aes(x=as.factor(year), ymin=lcl, ymax=ucl),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = psight_res, aes(x=as.factor(year), y=mean, group = var),
            linetype="dashed", size=0.5) +
  geom_point(data = psight_res, aes(x=as.factor(year), y=mean, fill = var, group = var),
             shape = 21, size = 3) +
  scale_fill_manual(values = c("black", "white"),
                    breaks=c("p", "osr"),
                    labels=c("James Bay", "Outside James Bay")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0, 1) +
  ylab("Detection probability") +
  theme_cust() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.85))

png(filename = "analysis-output/covariate-models/pca/figures/psight-plot.png",
    width=6, height=4, units="in", res=600)

plot(psight_plot)

dev.off()
