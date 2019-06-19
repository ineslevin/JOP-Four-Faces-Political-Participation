# Goal: Make Figures 4-6 and Table C1
# Dependencies: "datamodel.Rdata", "samples_stage2.Rdata"

setwd("~/Replication Package")

library(coda)
library(plotrix)
library(xtable)

# Load recoded data

load("datamodel.Rdata")

datamodel2 <- data.frame("education" = datamodel$education, "age" = datamodel$age, "female" = datamodel$female, "nonwhite" = datamodel$nonwhite, "income.proxy" = datamodel$income.proxy, "ideology" = datamodel$ideology, "natecon" = datamodel$natecon, "persfin" = datamodel$persfin, "victim" = datamodel$victim, "corrupt" = datamodel$corrupt, "capital" = datamodel$capital, "bigcity" = datamodel$bigcity, "y2012" = ifelse(datamodel$year.cat == 2, 1, 0), "y2014" = ifelse(datamodel$year.cat == 3, 1, 0))

# Standardize covariates
datamodel2 <- as.data.frame(scale(datamodel2))

# Load 2nd stage chains
load("samples_stage2.Rdata")

samples.stage2 <- as.mcmc.list(samples.stage2)

coef.samples <- samples.stage2[ , substr(names(samples.stage2[[1]][1, ]), 1, 6) != "beta[1"]

coef.samples.all <- NULL
for (j in 1:30) {
  coef.samples.all <- rbind(coef.samples.all, coef.samples[[j]])
}

N.iters <- dim(coef.samples.all)[1]

n.sims <- 15

X.median <- c(1, apply(datamodel2, 2, median))
names(X.median)[1] <- "constant"
X.median["female"] <- quantile(datamodel2$female)[2]
X.median["natecon"] <- quantile(datamodel2$natecon)[5]
X.median["persfin"] <- quantile(datamodel2$persfin)[5]
X.median["corrupt"] <- quantile(datamodel2$corrupt, p = c(0.05))

X.sims <- matrix(rep(X.median), nrow = n.sims, ncol = length(X.median), byrow = T)
colnames(X.sims) <- names(X.median)

X.sims[2, "education"] <- quantile(datamodel2$education)[4]
X.sims[3, "age"] <- quantile(datamodel2$age)[4]
X.sims[4, "female"] <- quantile(datamodel2$female)[4]
X.sims[5, "nonwhite"] <- quantile(datamodel2$nonwhite)[4]
X.sims[6, "income.proxy"] <- quantile(datamodel2$income.proxy)[4]
X.sims[7, "ideology"] <- quantile(datamodel2$ideology)[4]
X.sims[8, "natecon"] <- quantile(datamodel2$natecon)[2]
X.sims[9, "persfin"] <- quantile(datamodel2$persfin)[2]
X.sims[10, "victim"] <- quantile(datamodel2$victim)[5] 
X.sims[11, "corrupt"] <- quantile(datamodel2$corrupt)[4]
X.sims[12, "capital"] <- quantile(datamodel2$capital)[5]
X.sims[13, "bigcity"] <- quantile(datamodel2$bigcity)[5]
X.sims[14, "y2012"] <- quantile(datamodel2$y2012)[5]
X.sims[15, "y2014"] <- quantile(datamodel2$y2014)[5]

betas.list <- list()
eXB.list <- list()
P.list <- list()

for (j in 2:4) {

  betas.list[[j]] <- coef.samples.all[ , substr(colnames(coef.samples.all), 1, 6) == paste("beta[", j, sep="")]

  eXB.list[[j]] <- exp(betas.list[[j]] %*% t(X.sims))

}

P.list[[2]] <- eXB.list[[2]] / (matrix(1, nrow = N.iters, ncol = 15) + eXB.list[[2]] + eXB.list[[3]] + eXB.list[[4]])
P.list[[3]] <- eXB.list[[3]] / (matrix(1, nrow = N.iters, ncol = 15) + eXB.list[[2]] + eXB.list[[3]] + eXB.list[[4]])
P.list[[4]] <- eXB.list[[4]] / (matrix(1, nrow = N.iters, ncol = 15) + eXB.list[[2]] + eXB.list[[3]] + eXB.list[[4]])
P.list[[1]] <- matrix(1, nrow = N.iters, ncol = 15) - P.list[[2]] - P.list[[3]] - P.list[[4]]

Base.mean.list <- list()
Base.ql.list <- list()
Base.qu.list <- list()
dP.list <- list()
dP.mean.list <- list()
dP.ql.list <- list()
dP.qu.list <- list()

for (j in 1:4) {

  Base.mean.list[[j]] <- apply(P.list[[j]], 2, mean)

  Base.ql.list[[j]] <- apply(P.list[[j]], 2, quantile, p = 0.025)

  Base.qu.list[[j]] <- apply(P.list[[j]], 2, quantile, p = 0.975)

  dP.list[[j]] <- matrix(NA, nrow = N.iters, ncol = 14)

  for (k in 1:14) {
    dP.list[[j]][ , k] <- P.list[[j]][ , k + 1] - P.list[[j]][ , 1]
  }

  dP.mean.list[[j]] <- apply(dP.list[[j]], 2, mean)
  dP.ql.list[[j]] <- apply(dP.list[[j]], 2, quantile, p = 0.025)
  dP.qu.list[[j]] <- apply(dP.list[[j]], 2, quantile, p = 0.975)

  names(dP.mean.list[[j]]) <- names(X.median)[-1]
  names(dP.ql.list[[j]]) <- names(X.median)[-1]
  names(dP.qu.list[[j]]) <- names(X.median)[-1]

}

##-------- TABLE C1 --------##

METABLE <- cbind(
  dP.mean.list[[1]], dP.ql.list[[1]], dP.qu.list[[1]],
  dP.mean.list[[2]], dP.ql.list[[2]], dP.qu.list[[2]],
  dP.mean.list[[3]], dP.ql.list[[3]], dP.qu.list[[3]],
  dP.mean.list[[4]], dP.ql.list[[4]], dP.qu.list[[4]])

TableC1 <- rbind(c(Base.mean.list[[1]][1], Base.ql.list[[1]][1], Base.qu.list[[1]][1], Base.mean.list[[2]][1], Base.ql.list[[2]][1], Base.qu.list[[2]][1], Base.mean.list[[3]][1], Base.ql.list[[3]][1], Base.qu.list[[3]][1], Base.mean.list[[4]][1], Base.ql.list[[4]][1], Base.qu.list[[4]][1]), METABLE) * 100

colnames(TableC1) <- c("P(Outsider)", "2.5%", "97.5%", "P(Agitator)", "2.5%", "97.5%", "P(Conventional)", "2.5%", "97.5%", "P(Activist)", "2.5%", "97.5%")

xtable(TableC1, digits  =1)

##-------- FIGURE 4 --------##

# Effect economic evaluations

jpeg("Figure4.png", width = 800, height = 400)

par(mfrow=c(1, 2)) 
par(oma=c(4, 1, 4, 2),mar=c(0, 4, 0, 0)) 

plotCI(x = 1:4, y = TableC1["natecon", c(1, 4, 7, 10)], li = TableC1["natecon", c(2, 5, 8, 11)], ui = TableC1["natecon", c(3, 6, 9, 12)], xlim = c(0.5, 4.5), ylim = c(-15, 15), pch = 16, axes = FALSE, ann = FALSE)
axis(2, cex.axis = 1)
axis(1, at = 1:4, labels = c("Outsider", "Agitator", "Conventional", "Activist"), cex.axis = 1)
mtext("Concerns about National Economy",at = 2.5, line = 1, cex = 1.2)
abline(h = 0, col = "black")

plotCI(x = 1:4, y = TableC1["persfin", c(1, 4, 7, 10)], li = TableC1["persfin", c(2, 5, 8, 11)], ui = TableC1["persfin", c(3, 6, 9, 12)], xlim = c(0.5, 4.5), ylim = c(-15, 15), pch = 16, axes = FALSE, ann = FALSE)
axis(2, cex.axis = 1)
axis(1, at = 1:4, labels = c("Outsider", "Agitator", "Conventional", "Activist"), cex.axis = 1)
mtext("Concerns about Personal Economy",at = 2.5, line = 1, cex = 1.2)
abline(h = 0, col = "black")

mtext("Economic Evaluations", at = 0.46 , side = 3, line = -40, cex = 1, font = 2, outer = TRUE)

dev.off()

##-------- FIGURE 5 --------##

# Effect perceptions of corruption

jpeg("Figure5.png", width = 500, height = 400)

par(mfrow=c(1, 1)) 
par(oma=c(4, 1, 4, 2),mar=c(0, 4, 0, 0)) 

plotCI(x = 1:4, y = TableC1["corrupt", c(1, 4, 7, 10)], li = TableC1["corrupt", c(2, 5, 8, 11)], ui = TableC1["corrupt", c(3, 6, 9, 12)], xlim = c(0.5, 4.5), ylim = c(-15, 15), pch = 16, axes = FALSE, ann = FALSE)
axis(2, cex.axis = 1)
axis(1, at = 1:4, labels = c("Outsider", "Agitator", "Conventional", "Activist"), cex.axis = 1)
mtext("Perceptions of Corruption", at = 2.5, line = 1, cex = 1.2)
abline(h = 0, col = "black")

dev.off()

##-------- FIGURE 6 --------##

# Effect crime victimization

jpeg("Figure6.png", width = 500, height = 400)

par(mfrow=c(1, 1)) 
par(oma=c(4, 1, 4, 2),mar=c(0, 4, 0, 0)) 

plotCI(x = 1:4, y = TableC1["victim", c(1, 4, 7, 10)], li = TableC1["victim", c(2, 5, 8, 11)], ui = TableC1["victim", c(3, 6, 9, 12)], xlim = c(0.5, 4.5), ylim=c(-15, 15), pch = 16, axes = FALSE, ann = FALSE)
axis(2, cex.axis = 1)
axis(1, at = 1:4, labels = c("Outsider", "Agitator", "Conventional", "Activist"), cex.axis = 1)
mtext("Crime Victimization", at = 2.5, line = 1, cex = 1.2)
abline(h = 0, col = "black")

dev.off()
