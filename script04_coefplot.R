# Goal: Make Figure 3
# Dependencies: "samples_stage2.Rdata"

setwd("~/Replication Package")

library(coda)
library(plotrix)

load("samples_stage2.Rdata")

samples.stage2 <- as.mcmc.list(samples.stage2)

coef.samples <- samples.stage2[, substr(names(samples.stage2[[1]][1,]), 1, 6) != "beta[1"]

coef.samples.all30 <- NULL
for (j in 1:30) {
  coef.samples.all30 <- rbind(coef.samples.all30, coef.samples[[j]])
}

N.iters <- dim(coef.samples.all30)[1]

beta.samples.type2 <- coef.samples.all30[, substr(colnames(coef.samples.all30), 1, 6) == "beta[2"]
beta.samples.type3 <- coef.samples.all30[, substr(colnames(coef.samples.all30), 1, 6) == "beta[3"]
beta.samples.type4 <- coef.samples.all30[, substr(colnames(coef.samples.all30), 1, 6) == "beta[4"]

beta.means.type2 <- apply(beta.samples.type2, 2, mean)
beta.means.type3 <- apply(beta.samples.type3, 2, mean)
beta.means.type4 <- apply(beta.samples.type4, 2, mean)

beta.ql.type2 <- apply(beta.samples.type2, 2, quantile, p = 0.025)
beta.ql.type3 <- apply(beta.samples.type3, 2, quantile, p = 0.025)
beta.ql.type4 <- apply(beta.samples.type4, 2, quantile, p = 0.025)

beta.qu.type2 <- apply(beta.samples.type2, 2, quantile, p = 0.975)
beta.qu.type3 <- apply(beta.samples.type3, 2, quantile, p = 0.975)
beta.qu.type4 <- apply(beta.samples.type4, 2, quantile, p = 0.975)

##-------- FIGURE 3 --------##

varnames <- c("Education", "Age", "Female", "Non-white", "Income proxy", "Ideology (left-right scale)", "National economy", "Personal economy", "Crime victim", "Perception of corruption", "National capital", "Big city", "Year 2012", "Year 2014")

jpeg("Figure3.png", width = 850, height = 500)

par(mfrow = c(1, 3)) 
par(oma = c(5, 11, 0, 0), mar = c(0, 0, 4, 4)) 

# (L, H) vs. (L, L)

plotCI(y = 1:14 - 0.1, x = rev(beta.means.type2[2:15]), err = "x", ui = rev(beta.qu.type2[2:15]), li = rev(beta.ql.type2[2:15]), axes = FALSE, ann = FALSE, ylim = c(0.7, 14.1), xlim = c(-1.2, 1.2), pch = 16, pt.bg = par(cex = 0.75), sfrac = 0.005, lwd = 1, col = "black")
axis(side = 1 , cex.axis = 1)
axis(side = 2 , las = 1, at = c(1:14), cex.axis = 1,labels = rev(varnames), las = 2)
mtext("Agitator vs. Outsider", at = 0, line = 1, cex = 0.9)
abline(v = 0, col = "grey63")

# (H, L) vs. (L, L)

plotCI(y = 1:14 - 0.1, x = rev(beta.means.type3[2:15]), err = "x", ui = rev(beta.qu.type3[2:15]), li = rev(beta.ql.type3[2:15]), axes = FALSE, ann = FALSE, ylim = c(0.7, 14.1), xlim = c(-1.2, 1.2), pch = 16, pt.bg = par(cex = 0.75), sfrac = 0.005, lwd = 1, col = "black")
axis(side = 1, cex.axis = 1)
mtext("Conventional vs. Outsider",at = 0, line = 1, cex = 0.9)
abline(v=0,col="grey63")

# (H, H) vs. (L, L)

plotCI(y = 1:14 - 0.1, x = rev(beta.means.type4[2:15]), err = "x", ui = rev(beta.qu.type4[2:15]), li = rev(beta.ql.type4[2:15]), axes = FALSE, ann = FALSE, ylim = c(0.7, 14.1), xlim = c(-1.2, 1.2), pch = 16, pt.bg = par(cex = 0.75), sfrac = 0.005, lwd = 1, col = "black")
axis(side = 1, cex.axis = 1)
mtext("Activist vs. Outsider", at = 0, line = 1, cex = 0.9)
abline(v = 0, col = "grey63")

dev.off()
