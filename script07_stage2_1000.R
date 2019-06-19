# Goal: Estimate 2nd stage model (1000 times, each using a different sample from types' distribution)
# Dependencies: "datamodel.Rdata", "samples_stage1.Rdata", "stage2.bug"

setwd("~/Replication Package")

library(rjags)
library(coda)
load.module("glm")

library(foreach)
library(doMC)  
registerDoMC(12) 

RNGkind("L'Ecuyer-CMRG")

set.seed(20000)

load("datamodel.Rdata")

load("samples_stage1.Rdata")

conv.type.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 5) == "conv."], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 5) == "conv."], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 5) == "conv."])

unconv.type.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 5) == "uncon"], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 5) == "uncon"], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 5) == "uncon"])

N.draws <- 1000 # NUMBER OF DRAWS FROM TYPE DISTRIBUTION

tosample <- matrix(sample(1:3000, N.draws * dim(conv.type.samples)[2], replace = T), ncol = dim(conv.type.samples)[2])

conv.type.smallsamples <- matrix(NA, ncol = dim(conv.type.samples)[2], nrow = N.draws)
unconv.type.smallsamples <- matrix(NA, ncol = dim(unconv.type.samples)[2], nrow = N.draws)

for (i in 1:dim(conv.type.samples)[2]) {
  conv.type.smallsamples[ , i] <- conv.type.samples[tosample[ , i], i] - 1
  unconv.type.smallsamples[ , i] <- unconv.type.samples[tosample[ , i], i] - 1
}

##---------- ESTIMATE 2-STAGE MODEL ----------##

samples.stage2.J1000 <- foreach(j = 1:N.draws) %dopar% {

  conv.type <- conv.type.smallsamples[j, ]
  unconv.type <- unconv.type.smallsamples[j, ]
  
  typeComb <- NULL
  typeComb[conv.type == 0 & unconv.type == 0] <- 1
  typeComb[conv.type == 0 & unconv.type == 1] <- 2
  typeComb[conv.type == 1 & unconv.type == 0] <- 3
  typeComb[conv.type == 1 & unconv.type == 1] <- 4

  ncells <- 4

  nvar <- 15

  b0 <- rep(0, nvar)
  B0 <- diag(nvar) * 0.01

  datamodel2 <- data.frame("typeComb" = typeComb, "education" = datamodel$education, "age" = datamodel$age, "female" = datamodel$female, "nonwhite" = datamodel$nonwhite, "income.proxy" = datamodel$income.proxy, "ideology" = datamodel$ideology, "natecon" = datamodel$natecon, "persfin" = datamodel$persfin, "victim" = datamodel$victim, "corrupt" = datamodel$corrupt, "capital" = datamodel$capital, "bigcity" = datamodel$bigcity, "y2012" = ifelse(datamodel$year.cat == 2, 1, 0), "y2014" = ifelse(datamodel$year.cat == 3, 1, 0))

  # Standardize covariates
  datamodel2[, 2:15] <- scale(datamodel2[, 2:15])

  n2 <- dim(datamodel2)[1]
  
  data2.jags  <- list("n" = n2, "ncells" = ncells, "nvar" = nvar, "y2012" = datamodel2$y2012, "y2014" = datamodel2$y2014, "b0"= b0, "B0"= B0, "typeComb"=  datamodel2$typeComb, "education" = datamodel2$education, "age" = datamodel2$age, "female" = datamodel2$female, "nonwhite" = datamodel2$nonwhite, "income.proxy" = datamodel2$income.proxy, "ideology" = datamodel2$ideology, "natecon" = datamodel2$natecon, "persfin" = datamodel2$persfin, "victim" = datamodel2$victim, "corrupt" = datamodel2$corrupt, "capital" = datamodel2$capital, "bigcity" = datamodel2$bigcity)

  parameters2.jags <- c("beta")
  
  inits.jags <- list(list(.RNG.seed = sample(1:1000, 1), .RNG.name = "base::Mersenne-Twister"))

  model2.jags <- jags.model("stage2.bug", data = data2.jags, inits = inits.jags, n.chains = 1, n.adapt = 10000)
  
  mcmc.samples.stage2 <- coda.samples(model2.jags, variable.names = parameters2.jags, n.iter = 10000, thin = 10)

  mcmc.samples.stage2[[1]]

}

save(samples.stage2.J1000, file = "samples_stage2_J1000.Rdata")
