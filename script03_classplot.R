# Goal: Make Table 1 and Figures 1-2
# Dependencies: "samples_stage1.Rdata"

setwd("~/Replication Package")

library(xtable)
library(scales)
library(gplots)

load("samples_stage1.Rdata")

options(digits = 3)
options(scipen = 999)

# We first look at the results of the latent class model, usted to classify respondents into four participatory types:
  
# (1) Low conventional, low unconventional (Outsider)
# (2) Low conventional, high unconventional (Agitator)
# (3) High conventional, low unconventional (Conventional)
# (4) High conventional, high unconventional (Activist)

## Influence of participatory types on involvement in political activities

# Our expectation is that $\alpha_{C, j}$ should be higher for conventional activities, and that $\alpha_{U, j}$ should be higher for unconventional activites.

# To ensure identification of model parameters, we set $\alpha_{C, j}$ to zero for the activity a-priori considered to be the most unconventional one (blocking roads), and $\alpha_{U, j}$ to zero for the activity a-priori considered to be the most conventional one (attending meetings of municipal bodies). For the remaining activities, these parameters are allowed to vary freely.

# The following table give average values and 95% posterior intervals for all $\alpha_{C, j}$'s and $\alpha_{U, j}$'s:

act.names <- c("Municipal meetings", "Contact municipality", "Contact authorities", "Improvement meeting", "Party meeting", "Association meeting", "Solve problem", "Work for party", "Protest", "Strike", "Block")

alpha.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 6) == "alpha["], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 6) == "alpha["], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 6) == "alpha["])

alpha.conv.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 7) == "alpha.c"], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 7) == "alpha.c"], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 7) == "alpha.c"])

alpha.unconv.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 7) == "alpha.u"], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 7) == "alpha.u"], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 7) == "alpha.u"])

T.ac.i <- apply(alpha.conv.samples, 2, mean)
T.ac.ii <- apply(alpha.conv.samples, 2, quantile, p = c(0.025, 0.975))
T.ac <- t(rbind(T.ac.i, T.ac.ii))
colnames(T.ac) <- c("mean", "2.5%", "97.5%")
rownames(T.ac) <- act.names

T.au.i <- apply(alpha.unconv.samples, 2, mean)
T.au.ii <- apply(alpha.unconv.samples, 2, quantile, p = c(0.025, 0.975))
T.au <- t(rbind(T.au.i, T.au.ii))
colnames(T.au) <- c("mean", "2.5%", "97.5%")
rownames(T.au) <- act.names

##-------- TABLE 1 --------##

Table1 <- cbind(T.ac, T.au)

xtable(Table1)

# As expected, $\alpha_{C, j}$'s are higher for conventional activities than unconventional activites, and $\alpha_{U, j}$'s are higher for unconventional activities than conventional activites.

### Type Effects

# For each combination of conventional and unconventional types, we can compute predicted probabilities of participation in each activity.

inv.logit <- function(x) {
  y <- 1 / (1 + exp(-x))
  return(y)
}

n.iters <- dim(samples.stage1[[1]])[1] * 3
n.act <- dim(alpha.samples)[2]
n.ctypes <- 4
pred.P <- array(NA, c(n.iters, n.act, n.ctypes))
pred.change.P <- array(NA, c(n.iters, n.act, (n.ctypes - 1)))

pred.P[,,1] <- inv.logit(alpha.samples)
pred.P[,,2] <- inv.logit(alpha.samples + alpha.unconv.samples)
pred.P[,,3] <- inv.logit(alpha.samples + alpha.conv.samples)
pred.P[,,4] <- inv.logit(alpha.samples + alpha.conv.samples + alpha.unconv.samples)

pred.change.P[ , , 1] <- pred.P[ , , 2] - pred.P[ , , 1]
pred.change.P[ , , 2] <- pred.P[ , , 3] - pred.P[ , , 1]
pred.change.P[ , , 3] <- pred.P[ , , 4] - pred.P[ , , 1]

mean.prob <- apply(pred.P, c(2,3), mean)  * 100
rownames(mean.prob) <- act.names
quantile.prob.low <- apply(pred.P, c(2, 3), quantile, p = c(0.025))  * 100
quantile.prob.high <- apply(pred.P, c(2, 3), quantile, p = c(0.975))  * 100

mean.chprob <- apply(pred.change.P, c(2, 3), mean)  * 100
rownames(mean.chprob) <- act.names
quantile.chprob.low <- apply(pred.change.P, c(2, 3), quantile, p = c(0.025))  * 100
quantile.chprob.high <- apply(pred.change.P, c(2, 3), quantile, p = c(0.975))  * 100

T.probs <- cbind(mean.prob[, 1], quantile.prob.low[, 1], quantile.prob.high[, 1], mean.chprob[, 1], quantile.chprob.low[, 1], quantile.chprob.high[, 1], mean.chprob[, 2], quantile.chprob.low[, 2], quantile.chprob.high[, 2], mean.chprob[, 3], quantile.chprob.low[, 3], quantile.chprob.high[, 3])
colnames(T.probs) <- c("P(Y|T_LL)", "2.5%", "97.5%", "P(Y|T_LH) - P(Y|T_LL)", "2.5%", "97.5%", "P(Y|T_HL) - P(Y|T_LL)", "2.5%", "97.5%", "P(Y|T_HH) - P(Y|T_LL)", "2.5%", "97.5%")
rownames(T.probs) <- act.names

conv.order <- order(T.ac.i , decreasing = TRUE)

##-------- FIGURE 2 --------##

# The following bar plot gives participation probabilities for the four combined types:

jpeg("Figure2.png", width = 750, height = 500)
barplot2(height = mean.prob[conv.order,], beside = TRUE, ci.l = quantile.prob.low[conv.order,], ci.u = quantile.prob.high[conv.order,],col=rev(gray.colors(11)),ylim=c(0,100), plot.ci = TRUE, ci.color = "black", names.arg = c("Outsider", "Agitator", "Conventional", "Activist"), ylab = "Participation Probability")
axis(2)
legend(0, 101, rownames(mean.prob[conv.order, ]), cex = 0.8, bty = "n", fill = rev(gray.colors(11)))
mtext("Participation by Type",at = 24, line = 1, cex = 1)
dev.off()

# For each combined type, activities are sorted based on the extent to which they are affected by the conventional type (from higher $\alpha_{C, j}$ to lower $\alpha_{C, j}$). Political activities such as participation in protests, strikes, and road cloks tend to have low $\alpha_{C, j}$ and are therefore located toward the right of the spectrum.

## Type assignments

conv.type.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 5) == "conv."], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 5) == "conv."], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 5) == "conv."])

unconv.type.samples <- rbind(samples.stage1[[1]][ , substr(colnames(samples.stage1[[1]]), 1, 5) == "uncon"], samples.stage1[[2]][ , substr(colnames(samples.stage1[[2]]), 1, 5) == "uncon"], samples.stage1[[3]][ , substr(colnames(samples.stage1[[3]]), 1, 5) == "uncon"])

# Participatory types are not fixed for each individual; they are determined probabilistically.

prob.conv.type <- apply(conv.type.samples - 1, 2, mean) 
prob.unconv.type <- apply(unconv.type.samples - 1, 2, mean)

per.outsiders <- paste(round(mean(ifelse(prob.conv.type < 0.5 & prob.unconv.type < 0.5, 1, 0)) * 100, 1), "%", sep = "")
per.engaged <- paste(round(mean(ifelse(prob.conv.type >= 0.5 & prob.unconv.type < 0.5, 1, 0)) * 100, 1), "%", sep = "")
per.agitators <- paste(round(mean(ifelse(prob.conv.type < 0.5 & prob.unconv.type >= 0.5, 1, 0)) * 100, 1), "%", sep = "")
per.factotums <- paste(round(mean(ifelse(prob.conv.type >= 0.5 & prob.unconv.type >= 0.5, 1, 0)) * 100, 1), "%", sep = "")

##-------- FIGURE 1 --------##

# The following scatterplot gives the relationship between the probability of being assigned a high conventional type, and the probability of being assigned a high unconventional type. Each point in the plot corresponds to a survey respondent.

jpeg("Figure1.png", width = 600, height = 600)
plot(prob.conv.type, prob.unconv.type, xlab = "Probability of high conventional type",ylab = "Probability of high unconventional type", col = alpha("black", 0.3))
abline(h = 0.5, lty = 2)
abline(v = 0.5, lty = 2)
text(0.25, 0.25, paste("OUTSIDERS\n", "(",per.outsiders,")", sep = ""), col = "grey40")
text(0.75, 0.25, paste("CONVENTIONALS\n", "(",per.engaged,")", sep = ""), col = "grey40")
text(0.25, 0.75, paste("AGITATORS\n", "(",per.agitators,")", sep = ""), col = "grey40")
text(0.75, 0.75, paste("ACTIVISTS\n", "(",per.factotums,")", sep = ""), col = "grey40")
dev.off()

