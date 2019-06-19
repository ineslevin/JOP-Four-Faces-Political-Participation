# Goal: Make Figures A1-2
# Dependencies: "TurnoutByProvince.csv" and "TurnoutByPartidoBA.csv"

setwd("~/Replication Package")

# Load Data: 

data.prov <- read.csv("TurnoutByProvince.csv") # Loads turnout by Province in Presidential elections of  2007, 2011, and 2015
data.mun <- read.csv("TurnoutByPartidoBA.csv") # Loads turnout by Partido (municipalities) in Buenos Aires Province for Presidential Elections in 2007, 2011, and 2015

# Generate change in turnout between 2011 and 2007 (pre electoral reform) and 2015 versus 2015 (post electoral reform)
data.prov$pre.change <- data.prov$turnout2011 - data.prov$turnout2007
data.prov$post.change <- data.prov$turnout2015 - data.prov$turnout2011

data.mun$pre.change <- data.mun$turnout2011 - data.mun$turnout2007
data.mun$post.change <- data.mun$turnout2015 - data.mun$turnout2011

##-------- FIGURES A1 and A2 --------##

jpeg("FigureA1.png")
plot(data.prov$pre.change, data.prov$post.change, xlab = "Change between 2007 and 2011", ylab = "Change between 2011 and 2015",
     xlim = c(-1, 12), ylim = c(-1, 12), pch = 20)
abline(a = 0, b = 1, col = "grey63")
abline(a = 0, b = 0, col = "grey63")
abline(v = 0, col = "grey63")
points(data.prov$pre.change[data.prov$Provincia == "Total"], data.prov$post.change[data.prov$Provincia == "Total"], pch = 15)
dev.off()

jpeg("FigureA2.png")
plot(data.mun$pre.change, data.mun$post.change, xlab = "Change between 2007 and 2011", ylab = "Change between 2011 and 2015",
     xlim = c(-5, 10), ylim = c(-5, 10), pch = 20)
abline(a = 0, b = 1, col = "grey63")
abline(a = 0, b = 0, col = "grey63")
abline(v = 0, col = "grey63")
points(data.prov$pre.change[data.prov$Provincia == "Buenos Aires"], data.prov$post.change[data.prov$Provincia == "Buenos Aires"], pch = 15)
dev.off()

