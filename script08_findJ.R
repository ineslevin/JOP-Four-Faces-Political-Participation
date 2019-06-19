# Goal: Make Figure B1
# Dependencies: "samples_stage2_J1000.Rdata"

setwd("~/Replication Package")

library(coda)

load("samples_stage2_J1000.Rdata")

samples.stage2 <- as.mcmc.list(samples.stage2.J1000)

coef.samples <- samples.stage2[ , substr(names(samples.stage2[[1]][1,]), 1, 6) != "beta[1"]

N.samples <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

set.seed(1000)

seeds.vector <- sample(1:1000, 10)

avg.sd <- NULL
avg.width <- NULL

for (k in 1:length(seeds.vector)) {

  set.seed(seeds.vector[k])

  coef.subsamples <- list()

  for (i in 1:length(N.samples)) {
    samples.IDs  <- sample(1:1000, N.samples[i])
    coef.subsample <- NULL 
    for (j in 1:length(samples.IDs)) {
      coef.subsample <- rbind(coef.subsample, coef.samples[[samples.IDs[j]]])
    }
    coef.subsamples[[i]] <- coef.subsample
  }

  avg.sd <- cbind(avg.sd, unlist(lapply(coef.subsamples, function(x) mean(apply(x, 2, sd)))))
  avg.width <- cbind(avg.width, unlist(lapply(coef.subsamples, function(x) mean(apply(x, 2, quantile, p = 0.975) - apply(x, 2, quantile, p = 0.025)))))

  print(k)

}

averages <- list(avg.sd, avg.width)

##-------- FIGURE B1 --------##

jpeg("FigureB1.png", width = 500, height = 700)

par(mfrow =  c(2, 1)) 
par(oma =  c(4, 4, 0, 4), mar = c(0, 0, 6, 0)) 

plot.ts(avg.width, plot.type = "single", axes = F, ann = F, ylab = "Avg. s.d.", xlab = "J", col = "gray26", type = "p")
lines(apply(avg.width, 1, mean), lwd = 3)
axis(2)
axis(1, labels = N.samples, at = 1:length(N.samples))
mtext("Average s.d.", at = 6, line = 1, cex = 0.9)
mtext("J", side = 1, line = 2, cex.lab = 1, col = "black")

plot.ts(avg.sd, plot.type = "single", axes = F, ann = F, ylab = "Avg. c.i. width", xlab = "J", col = "gray26", type = "p")
lines(apply(avg.sd, 1, mean), lwd = 3)
axis(2)
axis(1, labels = N.samples, at = 1:length(N.samples))
mtext("Average width", at = 6, line = 1, cex = 0.9)
mtext("J", side = 1, line = 2, cex.lab = 1, col = "black")

dev.off()
